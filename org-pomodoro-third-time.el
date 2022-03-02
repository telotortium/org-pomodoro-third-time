;;; org-pomodoro-third-time.el --- Adapt ‘org-pomodoro' to implement Third Time -*- lexical-binding: t; -*-

;; Author: Robert Irelan <rirelan@gmail.com>
;; URL: https://github.com/telotortium/org-pomodoro-third-time
;; Created: Feb 8, 2022
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (org-pomodoro "2.1.0"))

;;; Commentary:

;; This package adapts the ‘org-pomodoro’ package to implement work and break
;; intervals from the Third Time system introduced in
;; https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work
;;
;; When this package is activated, ‘org-pomodoro-short-break-length’ is
;; automatically calculated to be ‘org-pomodoro-third-time-break-to-work-ratio’
;; times ‘org-pomodoro-length’. The length of the next break is adjusted based
;; on whether you start the next Pomodoro before or after the scheduled break
;; end.
;;
;; Long breaks are never scheduled automatically, but you can manually start
;; one, in which case any break time adjustments are reset.

;;; Code:

(require 'org-pomodoro)

;;; User-defined variables

(defgroup org-pomodoro-third-time nil
  "Implementation of Third Time for ‘org-pomodoro’.

See https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work"
  :group 'org)

(defcustom org-pomodoro-third-time-break-to-work-ratio (/ 1 3.0)
  "The ratio of break time to work time.

The default Third Break configuration sets this to 1/3 = 0.333..., which means
the `org-pomodoro' break is set to be one-third the length of the just finished
work period. If the Pomodoro has been ended early or later than
‘org-pomodoro-length', either by setting ‘org-pomodoro-manual-break’ or by
calling ‘org-pomodoro-end-in', the break time will be this fraction of the
modified Pomodoro time.

Must be a non-negative floating-point value."
  :group 'org-pomodoro-third-time
  :type 'float)

(defcustom org-pomodoro-third-time-minimum-break-length 1.0
  "The minimum break length, in minutes.

If you end a break early or late, the difference is accumulated in a “bank” and
added to the break length calculated using
‘org-pomodoro-third-time--validate-break-to-work-ratio'.

The bank can be positive or negative. For example, if the calculated break
length is 3 minutes, but you actually don’t start the next Pomodoro for 5
minutes, 2 minutes are subtracted from the bank. If the bank had 0 minutes, the
bank’s balance would then be -2 minutes.

If the bank is too negative, adding it to the default break length would produce
a break of very short or even negative length. This variable sets the minimum
break time in this situation.

The bank balance is set to 0 minutes after applying the bank balance, whether
the resulting break time is above or below this minimum."
  :group 'org-pomodoro-third-time
  :type 'float)

(defcustom org-pomodoro-third-time-end-in-default 10
  "Default to use when calling ‘org-pomodoro-third-time-end-in' interactively."
  :group 'org-pomodoro-third-time
  :type 'float)

(defvar org-pomodoro-third-time-long-break-hook nil
  "Hooks run by ‘org-pomodoro-third-time-long-break’ before starting \
a long break.

Modify this using ‘add-hook’ and ‘remove-hook’.")

(defvar org-pomodoro-third-time-modify-end-time-hook nil
  "Hooks run when changing the end time of the Pomodoro.

Run by ‘org-pomodoro-third-time-end-in’ and ‘org-pomodoro-third-time-end-at’
after changing the end time. Modify this using ‘add-hook’ and ‘remove-hook’.")

;;; Interactive commands

;;;###autoload
(defun org-pomodoro-third-time-long-break (minutes)
  "Start a long break immediately for MINUTES minutes.
This resets the bank."
  (interactive
   (list
    (read-number "Minutes for break: " org-pomodoro-long-break-length)))
  ;; Set Pomodoro state to :pomodoro so that ‘org-pomodoro-finished-hook’ is
  ;; run.
  (setq org-pomodoro-long-break-length minutes)
  (org-pomodoro-set :pomodoro)
  ;; Ensure ‘org-pomodoro-finished’ triggers a long break by manipulating the
  ;; count.
  (setq org-pomodoro-count -1)
  ;; Set the end time to now to immediately end the current Pomodoro.
  (org-pomodoro-third-time-end-in 0))

;;;###autoload
(defun org-pomodoro-third-time-end-in (minutes)
  "Force the current Pomodoro (or break) to end in MINUTES minutes.

If called interactively, prompt for MINUTES, with the default suggestion given
by ‘org-pomodoro-third-time-end-in-default'.

To end the Pomodoro immediately, call ‘org-pomodoro-third-time-end-now'."
  (interactive
   (list
    (read-number "End in how many minutes?: "
                 org-pomodoro-third-time-end-in-default)))
  (unless (org-pomodoro-active-p)
    (org-pomodoro))
  (setq org-pomodoro-end-time
        (time-add (current-time) (* minutes 60)))
  (run-hooks 'org-pomodoro-third-time-modify-end-time-hook))

;;;###autoload
(defun org-pomodoro-third-time-end-now ()
  "Force the current Pomodoro (or break) to end immediately."
  (interactive)
  (org-pomodoro-third-time-end-in 0)
  ;; Little extra work needed to end Pomodoro overtime
  (when org-pomodoro-manual-break
    (org-pomodoro-tick)                 ; Move to :overtime state
    (org-pomodoro)))                    ; End overtime Pomodoro

;;;###autoload
(defun org-pomodoro-third-time-end-at ()
  "Force the current Pomodoro (or break) to end at a user-prompted time."
  (interactive)
  (unless (org-pomodoro-active-p)
    (org-pomodoro))
  (setq org-pomodoro-end-time
        (org-read-date 'with-time 'to-time))
  (run-hooks 'org-pomodoro-third-time-modify-end-time-hook))

;;; Internal variables

(defvar org-pomodoro-third-time--bank-seconds 0
  "Number of seconds of unused rest time banked for future breaks.

Must always remain above 0.")

(defvar org-pomodoro-third-time--start-time nil
  "Start time of the current pomodoro or break.

Format: value returned by ‘encode-time’.")

(defvar org-pomodoro-third-time--expected-break-time nil
  "Expected length of break, in floating-point seconds.")

(defvar org-pomodoro-third-time--short-break-length nil
  "Original value of ‘org-pomodoro-short-break-length'.")
(defvar org-pomodoro-third-time--long-break-length nil
  "Original value of ‘org-pomodoro-long-break-length'.")
(defvar org-pomodoro-third-time--long-break-frequency nil
  "Original value of ‘org-pomodoro-long-break-frequency'.")

;;; Internal functions

(defun org-pomodoro-third-time--positive-float-p (x)
  "Assert X is a positive float."
  (and (floatp x) (>= x 0.0)))

(defun org-pomodoro-third-time--validate-break-to-work-ratio ()
  "Validate ‘org-pomodoro-third-time-break-to-work-ratio’."
  (unless (org-pomodoro-third-time--positive-float-p
           org-pomodoro-third-time-break-to-work-ratio)
    (user-error (concat
                 "‘org-pomodoro-third-time-break-to-work-ratio’ = %S, "
                 "should be a float >= 0")
                org-pomodoro-third-time-break-to-work-ratio)))

(defun org-pomodoro-third-time--set-start-time ()
  "Set ‘org-pomodoro-third-time--start-time’ upon Pomodoro start."
  (setq org-pomodoro-third-time--start-time (current-time)))

(defun org-pomodoro-third-time--set-break-length ()
  "Set break length for a short break."
  (org-pomodoro-third-time--validate-break-to-work-ratio)
  ;; The bank has no effect on the first break, so reset it in case it was not
  ;; reset earlier (in particular, after a long break or pomodoro killed).
  (when (= org-pomodoro-count 0)
    (org-pomodoro-third-time--reset-bank))
  (let* ((default-break-length
           (* org-pomodoro-third-time-break-to-work-ratio
              (float-time
               (time-subtract (current-time)
                              org-pomodoro-third-time--start-time))))
         (break-length
          (max (+ default-break-length
                  org-pomodoro-third-time--bank-seconds)
               (* 60 org-pomodoro-third-time-minimum-break-length))))
    (setq org-pomodoro-third-time--expected-break-time break-length)
    (setq org-pomodoro-short-break-length (/ break-length 60))
    ;; Set start time for when next pomodoro begins.
    (setq org-pomodoro-third-time--start-time (current-time))
    ;; Reset bank, since the bank has already been applied to this break.
    (org-pomodoro-third-time--reset-bank)))

(defun org-pomodoro-third-time--update-bank (fn state)
  "Update the bank if the break is shorter or longer than calculated.
Argument FN contains the original function advised by this.
Argument STATE contains the STATE argument passed to ‘org-pomodoro-start’."
  (when (and (or (null state)
                 (eq state :pomodoro))
             org-pomodoro-third-time--expected-break-time)
    (let* ((actual-break-time
            (float-time
             (time-subtract (current-time)
                            org-pomodoro-third-time--start-time)))
           (delta
            (- org-pomodoro-third-time--expected-break-time actual-break-time)))
      (message "actual-break-time %S delta %S" actual-break-time delta)
      (setq org-pomodoro-third-time--bank-seconds
            (+ org-pomodoro-third-time--bank-seconds delta))
      (message "bank seconds %S" org-pomodoro-third-time--bank-seconds)
      (setq org-pomodoro-third-time--expected-break-time nil)))
  (message "bank seconds %S" org-pomodoro-third-time--bank-seconds)
  (funcall fn state))

(defun org-pomodoro-third-time--reset-bank ()
  "Reset ‘org-pomodoro-third-time--bank-seconds’ to 0.0."
  (setq org-pomodoro-third-time--bank-seconds 0.0))

(defun org-pomodoro-third-time--reset-count ()
  "Reset ‘org-pomodoro-count’ to 0."
  (setq org-pomodoro-count 0))

;;; Minor mode definition

;;;###autoload
(define-minor-mode org-pomodoro-third-time-mode
  "Configure ‘org-pomodoro’ to use the rules of Third Time.

See https://www.lesswrong.com/posts/RWu8eZqbwgB9zaerh/third-time-a-better-way-to-work"
  :init-value nil
  :group 'org-pomodoro-third-time
  :global t
  :require 'org-pomodoro-third-time
  :lighter " ⅓tm"
  (cond (org-pomodoro-third-time-mode
         (message "Enabling third-time mode")
         ;; Store variables modified by this mode.
         (setq org-pomodoro-third-time--short-break-length
               org-pomodoro-short-break-length
               org-pomodoro-third-time--long-break-length
               org-pomodoro-long-break-length
               org-pomodoro-third-time--long-break-frequency
               org-pomodoro-long-break-frequency)
         ;; Add hooks and advice
         (add-hook 'org-pomodoro-started-hook
                   #'org-pomodoro-third-time--set-start-time)
         (add-hook 'org-pomodoro-long-break-finished-hook
                   #'org-pomodoro-third-time--reset-bank)
         (add-hook 'org-pomodoro-long-break-finished-hook
                   #'org-pomodoro-third-time--reset-count)
         (add-hook 'org-pomodoro-killed-hook
                   #'org-pomodoro-third-time--reset-bank)
         (add-hook 'org-pomodoro-killed-hook
                   #'org-pomodoro-third-time--reset-count)
         (advice-add 'org-pomodoro-start :around
                     #'org-pomodoro-third-time--update-bank)
         (advice-add 'org-pomodoro-finished :before
                     #'org-pomodoro-third-time--set-break-length)
         ;; Set variables
         (setq org-pomodoro-long-break-frequency most-positive-fixnum)
         (org-pomodoro-third-time--reset-bank))
        (t
         (message "Disabling third-time mode")
         ;; Restore variables modified by this mode.
         (unless (null org-pomodoro-third-time--short-break-length)
           (setq org-pomodoro-short-break-length
                 org-pomodoro-third-time--short-break-length)
           (setq org-pomodoro-third-time--short-break-length
                 nil))
         (unless (null org-pomodoro-third-time--long-break-length)
           (setq org-pomodoro-long-break-length
                 org-pomodoro-third-time--long-break-length)
           (setq org-pomodoro-third-time--long-break-length
                 nil))
         (unless (null org-pomodoro-third-time--long-break-frequency)
           (setq org-pomodoro-long-break-frequency
                 org-pomodoro-third-time--long-break-frequency)
           (setq org-pomodoro-third-time--long-break-frequency
                 nil))
         ;; Remove hooks and advice.
         (remove-hook 'org-pomodoro-started-hook
                      #'org-pomodoro-third-time--set-start-time)
         (remove-hook 'org-pomodoro-long-break-finished-hook
                      #'org-pomodoro-third-time--reset-bank)
         (remove-hook 'org-pomodoro-long-break-finished-hook
                      #'org-pomodoro-third-time--reset-count)
         (remove-hook 'org-pomodoro-killed-hook
                      #'org-pomodoro-third-time--reset-bank)
         (remove-hook 'org-pomodoro-killed-hook
                      #'org-pomodoro-third-time--reset-count)
         (advice-remove 'org-pomodoro-finished
                        #'org-pomodoro-third-time--set-break-length)
         (advice-remove 'org-pomodoro-start
                        #'org-pomodoro-third-time--update-bank)
         (advice-remove 'org-pomodoro-finished
                        #'org-pomodoro-third-time--set-break-length))))

(provide 'org-pomodoro-third-time)

;;; org-pomodoro-third-time.el ends here
