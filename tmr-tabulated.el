;;; tmr-tabulated.el --- Display timers in a tabulated list -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Damien Cassou <damien@cassou.me>,
;;         Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/tmr
;; Mailing list: https://lists.sr.ht/~protesilaos/tmr
;; Version: 0.3.1
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
;; Call `M-x tmr-tabulated-view' to display all tmr timers in a grid,
;; one by line with sortable columns.  Columns show the creation date,
;; the end date, a check mark if the timer is finished and the timer's
;; optional description.
;;
;; Please read the manual for all the technicalities.  Either evaluate
;; (info "(tmr) Top") or visit <https://protesilaos.com/emacs/tmr>.

;;; Code:

(require 'tmr)

;;;###autoload
(defun tmr-tabulated-view ()
  "Open a tabulated list buffer listing tmr timers."
  (interactive)
  (switch-to-buffer (get-buffer-create "*tmr-tabulated-view*"))
  (tmr-tabulated--set-entries)
  (tmr-tabulated-mode)
  (tabulated-list-print))

(defun tmr-tabulated--set-entries ()
  "Set the value of `tabulated-list-entries' with timers."
  (setq-local tabulated-list-entries
              (mapcar #'tmr-tabulated--timer-to-entry tmr--timers)))

(defun tmr-tabulated--timer-to-entry (timer)
  "Convert TIMER into an entry suitable for `tabulated-list-entries'."
  (list (tmr--timer-creation-date timer)
        (vector (tmr--format-creation-date timer)
                (tmr--format-end-date timer)
                (if (tmr--timer-donep timer) "✔" "")
                (or (tmr--timer-description timer) ""))))

(defvar tmr-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" #'tmr-cancel)
    (define-key map "K" #'tmr-remove-finished)
    (define-key map "+" #'tmr)
    (define-key map "t" #'tmr)
    (define-key map "*" #'tmr-with-description)
    (define-key map "T" #'tmr-with-description)
    (define-key map "c" #'tmr-clone)
    (define-key map "e" #'tmr-edit-description)
    (define-key map "s" #'tmr-reschedule)
    map)
  "Keybindings for `tmr-tabulated-mode-map'.")

(define-derived-mode tmr-tabulated-mode tabulated-list-mode "TMR"
  "Major mode to display tmr timers."
  (setq-local tabulated-list-format
              [("Start" 10 t)
               ("End" 10 t)
               ("Finished?" 10 t)
               ("Description" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'tmr-tabulated--set-entries nil t)
  (tabulated-list-init-header))

(defun tmr-tabulated--timer-at-point ()
  "Return the timer on the current line or nil."
  (and (eq major-mode #'tmr-tabulated-mode)
       (cl-find (tabulated-list-get-id) tmr--timers :key #'tmr--timer-creation-date)))

(defun tmr-tabulated--refresh ()
  "Refresh *tmr-tabulated-view* buffer if it exists."
  (when-let (buf (get-buffer "*tmr-tabulated-view*"))
    (with-current-buffer buf
      (let ((lines (line-number-at-pos)))
        (revert-buffer)
        (when (and (bobp) (> lines 1))
          (forward-line (1- lines))
          (unless (tabulated-list-get-id)
            (forward-line -1)))))))

(add-hook 'tmr--update-hook #'tmr-tabulated--refresh)
(add-hook 'tmr--read-timer-hook #'tmr-tabulated--timer-at-point)

(provide 'tmr-tabulated)
;;; tmr-tabulated.el ends here
