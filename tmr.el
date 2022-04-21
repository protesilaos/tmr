;;; tmr.el --- TMR Must Recur -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; TMR Must Recur.  Else a timer for my Emacs setup:
;; <https://protesilaos.com/dotemacs>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'notifications)

(defgroup tmr ()
  "TMR Must Recur (super simple timer for my private use)."
  :group 'data)

(defcustom tmr-sound-file
  "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"
  "Path to sound file used by `tmr--play-sound'."
  :type 'file
  :group 'tmr)

(defcustom tmr-notification-urgency 'normal
  "The urgency level of the desktop notification.
Values can be `low', `normal' (default), or `critical'.

The desktop environment or notification daemon is responsible for
such notifications."
  :type '(choice
          (const :tag "Low" low)
          (const :tag "Normal" normal)
          (const :tag "Critical" critical))
  :group 'tmr)

(defcustom tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread")
  "Optional description candidates for the current `tmr'."
  :type '(repeat string)
  :group 'tmr)

(defun tmr--unit (time)
  "Determine common time unit for TIME."
  (cond
   ((and (stringp time)
         (string-match-p "[0-9]\\'" time))
    (let ((time (string-to-number time)))
      (* time 60)))
   ((natnump time)
    (* time 60))
   (t
    (let* ((unit (substring time -1))
           (str (substring time 0 -1))
           (num (abs (string-to-number str))))
      (pcase unit
        ("s" num)
        ("h" (* num 60 60))
        ;; This is not needed, of course, but we should not miss a good
        ;; chance to make some fun of ourselves.
        ("w" (user-error "TMR Made Ridiculous; append character for [m]inutes, [h]ours, [s]econds"))
        (_ (* num 60)))))))

;; NOTE 2022-04-21: Emacs has a `play-sound' function but it only
;; supports .wav and .au formats.  Also, it does not work on all
;; platforms and Emacs needs to be compiled --with-sound capabilities.
(defun tmr--play-sound ()
  "Play `tmr-sound-file' using the 'ffplay' executable (ffmpeg)."
  (let ((sound tmr-sound-file))
    (when (file-exists-p sound)
      (unless (executable-find "ffplay")
        (user-error "Cannot play %s without `ffplay'" sound))
      (call-process-shell-command
       (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0))))

(defun tmr--log-in-buffer (log)
  "Insert LOG message in tmr buffer."
  (when-let ((buf (get-buffer-create "*tmr-messages*")))
    (with-current-buffer buf
      (messages-buffer-mode)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (concat log "\n"))))))

(defun tmr-view-echo-area-messages ()
  "View the '*tmr-messages*' buffer if present."
  (interactive)
  (if-let ((buf (get-buffer "*tmr-messages*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (let ((win (display-buffer (current-buffer))))
          ;; If the buffer is already displayed, we need to forcibly set
          ;; the window point to scroll to the end of the buffer.
          (set-window-point win (point))
          win))
    (user-error "No *tmr-messages* buffer; have you used `tmr'?")))

(defun tmr--notify-send (start &optional description)
  "Send system notification for timer with START time.
Optionally include DESCRIPTION."
  (let ((end (format-time-string "%T"))
        (desc-plain "")
        (desc-propertized ""))
    (when description
      (setq desc-plain (concat "\n" description)
            desc-propertized (concat " [" (propertize description 'face 'bold) "]")))
    ;; Read: (info "(elisp) Desktop Notifications")
    (notifications-notify
     :title "TMR Must Recur"
     :body (format "Time is up!\nStarted: %s\nEnded: %s%s"
                   start end desc-plain)
     :app-name "GNU Emacs"
     :urgency tmr-notification-urgency
     :sound-file tmr-sound-file)
    (message
     "TMR %s %s ; %s %s%s"
     (propertize "Start:" 'face 'success) start
     (propertize "End:" 'face 'warning) end
     desc-propertized)
    (unless (plist-get (notifications-get-capabilities) :sound)
      (tmr--play-sound))))

(defvar tmr--timers nil
  "List of timer objects.
Populated by `tmr' and then operated on by `tmr-cancel'.")

;;;###autoload
(defun tmr-cancel ()
  "Cancel timer object set with `tmr' command.
If there is a single timer, cancel it outright.  If there are
multiple timers, prompt for one with completion."
  (interactive)
  (if-let ((timers tmr--timers))
      (cond
       ((= (length timers) 1)
        (let ((cell (car timers)))
          (cancel-timer (cdr cell))
          (tmr--log-in-buffer (format "CANCELLED <<%s>>" (car cell)))
          (setq tmr--timers nil)))
       ((> (length timers) 1)
        (let* ((selection (completing-read "Cancel timer: " (mapc #'car timers) nil t))
               (cell (assoc selection timers #'string-match-p))
               (key (car cell))
               (object (cdr cell)))
          (cancel-timer object)
          (tmr--log-in-buffer (format "CANCELLED <<%s>>" key))
          (setq tmr--timers (delete cell tmr--timers)))))
    (user-error "No `tmr' to cancel")))

(defun tmr--echo-area (time &optional description)
  "Produce `message' for current `tmr' TIME.
Optionally include DESCRIPTION."
  (let* ((specifier (substring time -1))
         (amount (substring time 0 -1))
         (start (format-time-string "%T"))
         (unit (pcase specifier
                 ("s" (format "%ss (s == second)" amount))
                 ("h" (format "%sh (h == hour)" amount))
                 (_   (concat time "m (m == minute)")))))
    (message "`tmr' started at %s for %s%s"
             ;; Remember: these are just faces.  Don't get caught in the
             ;; semantics.
             (propertize start 'face 'success)
             (propertize unit 'face 'error)
             (if description
                 (format " [%s]" (propertize description 'face 'bold))
               ""))))

(defvar tmr--description-hist '()
  "Minibuffer history of `tmr' descriptions.")

(defun tmr--description-prompt ()
  "Helper prompt for descriptions in `tmr'."
  (let ((def (nth 0 tmr--description-hist)))
    (completing-read
     (format "Description for this tmr [%s]: " def)
     tmr-descriptions-list nil nil nil
     'tmr--description-hist def)))

;;;###autoload
(defun tmr (time &optional description)
  "Set timer to TIME duration and notify after it elapses.

When TIME is a number, it is interpreted as a count of minutes.
Otherwise TIME must be a string that consists of a number and a
special final character denoting a unit of time: 'h' for 'hours',
's' for 'seconds'.

With optional DESCRIPTION as a prefix (\\[universal-argument]),
prompt for a description among `tmr-descriptions-list', though
allow for any string to serve as valid input.

This command also plays back `tmr-sound-file'.

To cancel the timer, use the `tmr-cancel' command."
  (interactive
   (list
    (read-string "N minutes for timer (append `h' or `s' for other units): ")
    (when current-prefix-arg (tmr--description-prompt))))
  (let* ((start (format-time-string "%T"))
         (unit (tmr--unit time))
         (object-desc (if description
                          (format "Started at %s with input '%s' and description '%s'" start time description)
                        (format "Started at %s with input '%s'" start time))))
    (tmr--echo-area time description)
    (push (cons
           object-desc
           (run-with-timer
            unit nil
            'tmr--notify-send start description))
          tmr--timers)
    (tmr--log-in-buffer object-desc)))

(provide 'tmr)
;;; tmr.el ends here
