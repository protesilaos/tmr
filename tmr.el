;;; tmr.el --- Set timers using a convenient notation -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing list: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.2.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, timer

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
;; TMR is an Emacs package that provides facilities for setting timers
;; using a convenient notation.  The first point of entry is the `tmr'
;; command.  It prompts for a unit of time, which is represented as a
;; string that consists of a number and, optionally, a single character
;; suffix which specifies the unit of time.  Valid input formats:
;;
;; | Input | Meaning   |
;; |-------+-----------|
;; | 5     | 5 minutes |
;; | 5m    | 5 minutes |
;; | 5s    | 5 seconds |
;; | 5h    | 5 hours   |
;;
;; If `tmr' is called with an optional prefix argument (`C-u'), it also
;; asks for a description which accompanies the given timer.  Preconfigured
;; candidates are specified in the user option `tmr-descriptions-list',
;; though any arbitrary input is acceptable at the minibuffer prompt.
;;
;; An alternative to the `tmr' command is `tmr-with-description'.  The
;; difference between the two is that the latter always prompts for a
;; description.
;;
;; When the timer is set, a message is sent to the echo area recording the
;; current time and the point in the future when the timer elapses.  Echo
;; area messages can be reviewed with the `view-echo-area-messages' which
;; is bound to `C-h e' by default.  To check all timers, use the command
;; `tmr-tabulated-view', which is described further below.
;;
;; Once the timer runs its course, it produces a desktop notification and
;; plays an alarm sound.  The notification's message is practically the
;; same as that which is sent to the echo area.  The sound file for the
;; alarm is defined in `tmr-sound-file', while the urgency of the
;; notification can be set through the `tmr-notification-urgency' option.
;; Note that it is up to the desktop environment or notification daemon to
;; decide how to handle the urgency value.
;;
;; If the `tmr-sound-file' is nil, or the file is not found, no sound will
;; be played.
;;
;; The `tmr-cancel' command is used to cancel running timers (as set by the
;; `tmr' or `tmr-with-description' commands).  If there is only one timer,
;; it cancels it outright.  If there are multiple timers, it produces a
;; minibuffer completion prompt which asks for one among them.  Timers at
;; the completion prompt are described by the exact time they were set and
;; the input that was used to create them, including the optional
;; description that `tmr' and `tmr-with-description' accept.
;;
;; An existing timer can be cloned with the `tmr-clone' command.  It copies
;; the duration and optional description of an existing timer into a new
;; one.
;;
;; Active timers can be viewed in a grid with `tmr-tabulated-view' (part of
;; the `tmr-tabulated.el' file).  The grid is placed in the
;; `*tmr-tabulated-view*' buffer and looks like this:
;;
;; Start      End        Finished?  Description
;; 12:26:50   12:51:50   ✔         Update tmr manual
;; 12:26:35   12:56:35              Bake bread
;; 12:26:26   12:36:26              Prepare tea
;;
;; If a timer has elapsed, it has a check mark, otherwise the `Finished?'
;; column is empty.  A `Description' is shown only if it is provided while
;; setting the timer, otherwise the field is left blank.
;;
;; The `tmr-tabulated-view' command relies on Emacs' `tabulated-list-mode'.
;; From the `*tmr-tabulated-view*' buffer, invoke the command
;; `describe-mode' to learn about the applicable key bindings, such as how
;; to expand/contract columns and toggle their sort.
;;
;; While in this grid view:
;;
;; + The `+' key creates a new timer by calling the standard `tmr' command.
;;   As always, use a prefix argument to also prompt for a description.
;;
;; + The `c' key invokes the `tmr-tabulated-cancel' command.  It is the
;;   same as the aforementioned `tmr-cancel' plus some tweaks for the grid
;;   view.
;;
;; + The `k' key runs the `tmr-tabulated-cancel' command.  It immediately
;;   cancels the timer at point.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup tmr ()
  "TMR May Ring: set timers using a simple notation."
  :group 'data)

(defcustom tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread")
  "Optional description candidates for the current `tmr'.
These are provided as completion candidates when `tmr' is called
with a DESCRIPTION argument or when `tmr-with-description' is
used."
  :type '(repeat string)
  :group 'tmr)

(defcustom tmr-timer-created-functions
  (list #'tmr-print-message-for-created-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook
  :options '(tmr-print-message-for-created-timer))

(declare-function tmr-sound-play "ext:tmr-sound.el")
(declare-function tmr-notification-notify "ext:tmr-notification.el")

(defcustom tmr-timer-completed-functions
  (list #'tmr-print-message-for-completed-timer
        #'tmr-sound-play
        #'tmr-notification-notify)
  "Functions to execute when a timer is completed.
Each function must accept a timer as argument."
  :type 'hook
  :options (list #'tmr-print-message-for-completed-timer
                 #'tmr-sound-play
                 #'tmr-notification-notify))

(defcustom tmr-timer-cancelled-functions
  (list #'tmr-print-message-for-cancelled-timer)
  "Functions to execute when a timer is created.
Each function must accept a timer as argument."
  :type 'hook)

(cl-defstruct (tmr-timer
               (:constructor tmr--timer-create)
               (:conc-name tmr--timer-))
  (creation-date
   nil
   :read-only t
   :documentation "Time at which the timer was created.")
  (duration
   nil
   :read-only t
   :documentation "Number of seconds after `start' indicating when the timer finishes.")
  (donep
   nil
   :read-only nil
   :documentation "Non-nil if the timer is finished.")
  (timer-object
   nil
   :read-only nil
   :documentation "The object returned by `run-with-timer'.")
  (input
   nil
   :read-only t
   :documentation "The original input which is internally interpreted as a duration.")
  (description
   nil
   :read-only t
   :documentation "Optional string describing the purpose of the timer, e.g., \"Stop the oven\"."))

(defun tmr--long-description (timer)
  "Return a human-readable description for TIMER."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer)))
    ;; We prefix it with TMR just so it is easier to find in
    ;; `view-echo-area-messages'.  The concise wording makes it flexible
    ;; enough to be used when starting a timer but also when cancelling
    ;; one: check `tmr-print-message-for-created-timer' and
    ;; `tmr-print-message-for-cancelled-timer'.
    (format "TMR start at %s; end at %s%s"
            (propertize start 'face 'success)
            (propertize end 'face 'error)
            (if description
                (format " [%s]" (propertize description 'face 'bold))
              ""))))

(defun tmr--long-description-for-completed-timer (timer)
  "Return a human-readable description of completed TIMER.
This includes the creation and completion dates as well as the
optional `tmr--timer-description'."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer)))
    ;; For the TMR prefix, see comment in `tmr--long-description'.
    (format "TMR Time is up!\n%s%s %s\n%s %s"
            (if description (format "%s\n" description) "")
            (propertize "Started" 'face 'success)
            start
            (propertize "Ended" 'face 'error)
            end)))

(defun tmr--format-creation-date (timer)
  "Return a string representing when TIMER was created."
  (tmr--format-time (tmr--timer-creation-date timer)))

(defun tmr--format-end-date (timer)
  "Return a string representing when TIMER should finish."
  (format-time-string "%T" (time-add (tmr--timer-creation-date timer)
                                     (tmr--timer-duration timer))))

(defun tmr--format-time (time)
  "Return a human-readable string representing TIME."
  (format-time-string "%T" time))

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

(defvar tmr--timers nil
  "List of timer objects.
Populated by `tmr' and then operated on by `tmr-cancel'.")

(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-delete "cl-seq" (cl-item cl-seq &rest cl-keys))
(declare-function cl-remove-if "cl-seq" (cl-pred cl-list &rest cl-keys))

(defun tmr--active-timers ()
  "Retun list of active timers."
  (cl-remove-if
   (lambda (timer)
     (tmr--timer-donep timer))
   tmr--timers))

(defun tmr--get-timer-by-creation-date (creation-date)
  "Return the timer which was started at CREATION-DATE."
  (cl-find creation-date tmr--timers :key #'tmr--timer-creation-date))

;;;###autoload
(defun tmr-cancel (timer &optional no-hooks)
  "Cancel TIMER object set with `tmr' command.
Interactively, let the user choose which timer to cancel with
completion.

With optional NO-HOOKS refrain from calling
`tmr-timer-cancelled-functions'."
  (interactive (list (tmr--read-timer :active) current-prefix-arg))
  (if (not timer)
      (user-error "No `tmr' to cancel")
    (cancel-timer (tmr--timer-timer-object timer))
    (setq tmr--timers (cl-delete timer tmr--timers))
    (unless no-hooks
      (run-hook-with-args 'tmr-timer-cancelled-functions timer))))

(defun tmr--read-timer (&optional active)
  "Let the user choose a timer among all timers.
Return the selected timer.  If there is a single timer, use that.
If there are multiple timers, prompt for one with completion.  If
there are no timers, return nil.

If optional ACTIVE is non-nil, limit the list of timers to those
that are still running."
  (let ((timers (if active (tmr--active-timers) tmr--timers)))
    (cond
     ((= (length timers) 1)
      (car timers))
     ((> (length timers) 1)
      (let* ((timer-descriptions (mapcar #'tmr--long-description timers))
             (selection (completing-read "Timer: " timer-descriptions nil t)))
        (cl-find selection timers :test #'string= :key #'tmr--long-description))))))

(defun tmr-print-message-for-created-timer (timer)
  "Show a `message' informing the user that TIMER was created."
  (message "%s" (tmr--long-description timer)))

(defun tmr-print-message-for-completed-timer (timer)
  "Show a `message' informing the user that TIMER has completed."
  (message "%s" (tmr--long-description-for-completed-timer timer)))

(defun tmr-print-message-for-cancelled-timer (timer)
  "Show a `message' informing the user that TIMER is cancelled."
  (message "Cancelled: <<%s>>" (tmr--long-description timer)))

(defvar tmr--duration-hist '()
  "Minibuffer history of `tmr' durations.")

(defun tmr--read-duration (&optional default)
  "Ask the user to type a duration.
If DEFAULT is provided, use that as a default."
  (let ((def (or default (nth 0 tmr--duration-hist))))
    (read-string
     (if def
         (format "N minutes for timer (append `h' or `s' for other units) [%s]: " def)
       "N minutes for timer (append `h' or `s' for other units): ")
     nil
     'tmr--duration-hist def)))

(defvar tmr--description-hist '()
  "Minibuffer history of `tmr' descriptions.")

(defun tmr--description-prompt (&optional default)
  "Helper prompt for descriptions in `tmr'.
If optional DEFAULT is provided use it as a default.  Otherwise
use the latest input from the `tmr--description-hist', if
present."
  (let ((def (or default (nth 0 tmr--description-hist))))
    (completing-read
     (if def
         (format "Description for this tmr [%s]: " def)
       "Description for this tmr: ")
     tmr-descriptions-list nil nil nil
     'tmr--description-hist def)))

(defun tmr--complete (timer)
  "Mark TIMER as completed and execute `tmr-timer-completed-functions'."
  (setf (tmr--timer-donep timer) t)
  (run-hook-with-args 'tmr-timer-completed-functions timer))

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

This command also plays back `tmr-sound-file' if it is available.

To cancel the timer, use the `tmr-cancel' command.

To always prompt for a DESCRIPTION when setting a timer, use the
command `tmr-with-description' instead of this one."
  (interactive
   (list
    (tmr--read-duration)
    (when current-prefix-arg (tmr--description-prompt))))
  (let* ((creation-date (current-time))
         (duration (tmr--unit time))
         (timer (tmr--timer-create
                 :description description
                 :creation-date creation-date
                 :duration duration
                 :input time))
         (timer-object (run-with-timer
                        duration nil
                        #'tmr--complete timer)))
    (setf (tmr--timer-timer-object timer) timer-object)
    (push timer tmr--timers)
    (run-hook-with-args 'tmr-timer-created-functions timer)))

;;;###autoload
(defun tmr-with-description (time description)
  "Set timer to TIME duration and notify with DESCRIPTION after it elapses.

See `tmr' for a description of the arguments.  The difference
between the two commands is that `tmr-with-description' always
asks for a description whereas `tmr' only asks for it when the
user uses a prefix argument (\\[universal-argument])."
  (interactive
   (list
    (tmr--read-duration)
    (tmr--description-prompt)))
  (tmr time description))

;;;###autoload
(defun tmr-clone (timer &optional prompt)
  "Create a new timer by cloning TIMER.
With optional PROMPT, such as a prefix argument, ask for
confirmation about the duration and the description.  The
description is asked only if TIMER had one.

Without a PROMPT, clone TIMER outright."
  (interactive (list (tmr--read-timer) current-prefix-arg))
  (let ((description (tmr--timer-description timer)))
    (cond
     (prompt
      (tmr
       (tmr--read-duration (format "%s" (tmr--timer-input timer)))
       (when description (tmr--description-prompt description))))
     (t
      (tmr
       (format "%s" (tmr--timer-input timer))
       (tmr--timer-description timer))))))

(provide 'tmr)
;;; tmr.el ends here
