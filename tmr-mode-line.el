;;; tmr-mode-line.el --- Mode-line integration for tmr -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;;; Commentary:

;; This package provides a mode-line component that displays active TMR May Ring
;; timers with a countdown to when they fire.
;;
;; To use it, add the following to your init file:
;;
;; (require 'tmr-mode-line)
;; (tmr-mode-line-mode 1)
;;
;; Customize the appearance with:
;; - `tmr-mode-line-format': Format string for displaying each timer
;; - `tmr-mode-line-separator': String used to separate multiple timers
;; - `tmr-mode-line-max-timers': Maximum number of timers to display
;; - `tmr-mode-line-max-desc-length': Max length for timer descriptions

;;; Code:

(require 'tmr)
(require 'format-spec)
(eval-when-compile (require 'subr-x))

(defgroup tmr-mode-line nil
  "Mode-line integration for TMR May Ring."
  :link '(info-link :tag "Info Manual" "(tmr)")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/tmr")
  :link '(emacs-library-link :tag "Library Source" "tmr.el")
  :group 'tmr)

(defcustom tmr-mode-line-format "%r%d"
  "Format string for displaying a timer in the mode-line.
Available format specifiers:
- %r: Remaining time.
- %d: Timer description (truncated to `tmr-mode-line-max-desc-length')."
  :type 'string
  :group 'tmr-mode-line)

(defcustom tmr-mode-line-separator " | "
  "String used to separate multiple timers in the mode-line."
  :type 'string
  :group 'tmr-mode-line)

(defcustom tmr-mode-line-max-timers 3
  "Maximum number of timers to display in the mode-line.
Set to nil to show all timers."
  :type '(choice (const :tag "Show all" nil)
                 (integer :tag "Maximum number"))
  :group 'tmr-mode-line)

(defcustom tmr-mode-line-max-desc-length 15
  "Maximum length for timer descriptions in the mode-line.
Longer descriptions will be truncated."
  :type '(choice (const :tag "Don't truncate" nil)
                 (integer :tag "Truncate"))
  :group 'tmr-mode-line)

(defcustom tmr-mode-line-prefix "⏰"
  "Prefix string displayed before the timer list."
  :type 'string
  :group 'tmr-mode-line)

(defface tmr-mode-line-active
  '((t :inherit mode-line-emphasis))
  "Face for active timers in the mode-line."
  :group 'tmr-mode-line)

(defface tmr-mode-line-soon
  '((t :inherit warning))
  "Face for timers that will expire in the next 2 minutes."
  :group 'tmr-mode-line)

(defface tmr-mode-line-urgent
  '((t :inherit error))
  "Face for timers that will expire in the next 30 seconds."
  :group 'tmr-mode-line)

(defvar tmr-mode-line-string nil
  "TMR mode-line string.")
(put 'tmr-mode-line-string 'risky-local-variable t)

(defvar tmr-mode-line--update-timer nil
  "Timer to update the mode-line.")

(defun tmr-mode-line--format-remaining (timer)
  "Format remaining time for TIMER with appropriate face."
  (let* ((secs (float-time (time-subtract (tmr--timer-end-date timer) nil)))
         (face (cond ((and (< secs 5) (evenp (truncate secs)))
                      '((t :inherit tmr-mode-line-urgent :inverse-video t)))
                     ((< secs 30) 'tmr-mode-line-urgent)
                     ((= (truncate secs) 30)
                      '((t :inherit tmr-mode-line-urgent :inverse-video t)))
                     ((= (truncate secs) 60)
                      '((t :inherit tmr-mode-line-soon :inverse-video t)))
                     ((< secs 120) 'tmr-mode-line-soon)
                     ((= (truncate secs) 120)
                      '((t :inherit tmr-mode-line-soon :inverse-video t)))
                     (t 'tmr-mode-line-active)))
         (formatted (format-seconds
                     (cond ((< secs 120) "%mm %ss%z")
                           ((< secs (* 24 60 60)) "%hh %mm%z")
                           (t "%dd %hh%z"))
                     secs)))
    (propertize formatted 'face face)))

(defun tmr-mode-line--format-description (timer)
  "Format description for TIMER, truncating if necessary."
  (if-let* ((desc (tmr--timer-description timer)))
    (concat " " (if tmr-mode-line-max-desc-length
                    (truncate-string-to-width
                     desc tmr-mode-line-max-desc-length
                     nil nil t)
                  desc))
    ""))

(defun tmr-mode-line--format-timer (timer)
  "Format a single TIMER for display in the mode-line."
  (propertize
   (format-spec tmr-mode-line-format
                `((?r . ,(tmr-mode-line--format-remaining timer))
                  (?d . ,(tmr-mode-line--format-description timer))))
   'help-echo (tmr--long-description timer)))

(defun tmr-mode-line--get-active-timers ()
  "Return a sorted list of active timers."
  (thread-last tmr--timers
               (seq-remove #'tmr--timer-finishedp)
               (seq-sort-by #'tmr--timer-end-date #'time-less-p)))

(defun tmr-mode-line--update ()
  "Updates `tmr-mode-line-string' based on the current timer state."
  (setq
   tmr-mode-line-string
   (if-let* ((active-timers (tmr-mode-line--get-active-timers)))
       (let* ((truncate (and tmr-mode-line-max-timers
                             (length> active-timers tmr-mode-line-max-timers)))
              (timers-to-show (if truncate
                                 (seq-take active-timers
                                           tmr-mode-line-max-timers)
                                active-timers)))
         (concat
          " " tmr-mode-line-prefix " "
          (string-join (mapcar #'tmr-mode-line--format-timer timers-to-show)
                       tmr-mode-line-separator)
          (when truncate
            (format " +%d" (- (length active-timers) tmr-mode-line-max-timers)))
          " "))
     "")))

(defun tmr-mode-line-update ()
  "Update the mode line with current timer information."
  (tmr-mode-line--update)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode tmr-mode-line-mode
  "Display TMR May Ring timers in the global mode line."
  :global t
  :group 'tmr-mode-line
  (if tmr-mode-line-mode
      (progn
        (unless global-mode-string (setq global-mode-string '("")))
        (unless (memq 'tmr-mode-line-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(tmr-mode-line-string))))
        (setq tmr-mode-line--update-timer
              (run-at-time t 1 #'tmr-mode-line-update))
        (add-hook 'tmr--update-hook #'tmr-mode-line-update))
    (when tmr-mode-line--update-timer
      (cancel-timer tmr-mode-line--update-timer)
      (setq tmr-mode-line--update-timer nil))
    (setq tmr-mode-line-string nil)
    (remove-hook 'tmr--update-hook #'tmr-mode-line-update)))

(provide 'tmr-mode-line)
;;; tmr-mode-line.el ends here
