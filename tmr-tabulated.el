;;; tmr-tabulated.el --- Display TMR timers in a tabulated list -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>,
;;         Damien Cassou <damien@cassou.me>,
;;         Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/tmr

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

;; NOTE 2024-10-30: We keep this file to avoid breaking user
;; configurations, but we will remove it eventually.
(display-warning
 'tmr
 "`tmr-tabulated.el' is merged into `tmr.el'; only use `tmr.el'"
 :warning)

(provide 'tmr-tabulated)
;;; tmr-tabulated.el ends here
