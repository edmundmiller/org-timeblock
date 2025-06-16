;;; org-timeblock-modes.el --- Modes for org-timeblock -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1") (org "9.0") (svg "1.1"))
;; Keywords: org, calendar, timeblocking, agenda
;; URL: https://github.com/ichernyshovvv/org-timeblock

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the mode definitions, keymaps, and Evil
;; integration for org-timeblock.

;;; Code:

(require 'org-timeblock-config)
(require 'org-timeblock-util)
(require 'org-timeblock-org)
(require 'org-timeblock-draw)

;;;; Keymaps

(defvar-keymap org-timeblock-mode-map
  "+" #'org-timeblock-new-task
  "<mouse-1>" #'org-timeblock-select-block-with-cursor
  "<down>" #'org-timeblock-forward-block
  "n" #'org-timeblock-forward-block
  "<up>" #'org-timeblock-backward-block
  "p" #'org-timeblock-backward-block
  "<right>" #'org-timeblock-forward-column
  "f" #'org-timeblock-forward-column
  "<left>" #'org-timeblock-backward-column
  "b" #'org-timeblock-backward-column
  "C-<right>" #'org-timeblock-day-later
  "C-f" #'org-timeblock-day-later
  "C-<left>" #'org-timeblock-day-earlier
  "C-b" #'org-timeblock-day-earlier
  "RET" #'org-timeblock-goto
  "TAB" #'org-timeblock-goto-other-window
  "d" #'org-timeblock-set-duration
  "i" #'org-timeblock-clock-in
  "o" #'org-clock-out
  "g" #'org-timeblock-redraw-buffers
  "j" #'org-timeblock-jump-to-day
  "C-s" #'org-save-all-org-buffers
  "s" #'org-timeblock-schedule
  "T" #'org-timeblock-toggle-timeblock-list
  "t" #'org-timeblock-todo
  "v" #'org-timeblock-switch-scaling
  "V" #'org-timeblock-change-span
  "m" #'org-timeblock-mark-block
  "%" #'org-timeblock-mark-by-regexp
  "u" #'org-timeblock-unmark-block
  "U" #'org-timeblock-unmark-all-blocks
  "w" #'org-timeblock-write)

(defvar-keymap org-timeblock-list-mode-map
  "+" #'org-timeblock-new-task
  "<remap> <next-line>" #'org-timeblock-list-next-line
  "n" #'org-timeblock-list-next-line
  "<remap> <previous-line>" #'org-timeblock-list-previous-line
  "p" #'org-timeblock-list-previous-line
  "C-<right>" #'org-timeblock-day-later
  "f" #'org-timeblock-day-later
  "C-<left>" #'org-timeblock-day-earlier
  "b" #'org-timeblock-day-earlier
  "C-s" #'org-save-all-org-buffers
  "RET" #'org-timeblock-list-goto
  "TAB" #'org-timeblock-list-goto-other-window
  "d" #'org-timeblock-list-set-duration
  "i" #'org-timeblock-list-clock-in
  "o" #'org-clock-out
  "g" #'org-timeblock-redraw-buffers
  "j" #'org-timeblock-jump-to-day
  "q" #'org-timeblock-quit
  "s" #'org-timeblock-list-schedule
  "T" #'org-timeblock-list-toggle-timeblock
  "t" #'org-timeblock-todo
  "v" #'org-timeblock-switch-scaling
  "V" #'org-timeblock-change-span)

;;;; Modes

(define-derived-mode org-timeblock-mode
  special-mode "Org-Timeblock" :interactive nil
  (setq
   org-timeblock-daterange
	(cons (decode-time)
	      (org-timeblock-time-inc 'day (1- org-timeblock-span)
				 (decode-time)))
   cursor-type nil
   buffer-read-only t)
  (org-timeblock-redisplay))

(define-derived-mode org-timeblock-list-mode
  special-mode "Org-Timeblock-List" :interactive nil
  (setq truncate-lines t))

(require 'org-timeblock-evil nil t)

(provide 'org-timeblock-modes)

;;; org-timeblock-modes.el ends here
