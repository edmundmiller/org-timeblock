;;; org-timeblock-evil.el --- Evil integration for org-timeblock -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.2
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1") (org "9.0") (evil "1.14.0"))
;; Keywords: org, calendar, timeblocking, agenda, evil
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

;; This file contains Evil integration for org-timeblock.

;;; Code:

(require 'evil)
(require 'org-timeblock-modes)
(require 'org-timeblock-config)

;;;; Evil Integration

(defun org-timeblock-setup-evil-keybindings ()
  "Set up Evil keybindings for org-timeblock modes."
  (when (and (featurep 'evil) org-timeblock-evil-keybindings)
    ;; org-timeblock-mode Evil keybindings
    (evil-define-key 'normal org-timeblock-mode-map
      ;; Vim-like navigation
      "j" 'org-timeblock-forward-block
      "k" 'org-timeblock-backward-block
      "h" 'org-timeblock-backward-column
      "l" 'org-timeblock-forward-column
      ;; Alternative navigation (original keybindings)
      "n" 'org-timeblock-forward-block
      "p" 'org-timeblock-backward-block
      "f" 'org-timeblock-forward-column
      "b" 'org-timeblock-backward-column
      ;; Day navigation
      "C-f" 'org-timeblock-day-later
      "C-b" 'org-timeblock-day-earlier
      ;; Scrolling (Evil-like)
      "C-d" 'org-timeblock-day-later
      "C-u" 'org-timeblock-day-earlier
      ;; Entry actions
      (kbd "RET") 'org-timeblock-goto
      (kbd "TAB") 'org-timeblock-goto-other-window
      "o" 'org-timeblock-goto-other-window
      ;; Editing
      "d" 'org-timeblock-set-duration
      "i" 'org-timeblock-clock-in
      "O" 'org-clock-out
      ;; Refresh and navigation
      "gr" 'org-timeblock-redraw-buffers
      "gj" 'org-timeblock-jump-to-day
      "gg" 'org-timeblock-jump-to-day
      ;; Scheduling and todos
      "s" 'org-timeblock-schedule
      "T" 'org-timeblock-toggle-timeblock-list
      "t" 'org-timeblock-todo
      ;; View options
      "v" 'org-timeblock-switch-scaling
      "V" 'org-timeblock-change-span
      ;; Marking
      "m" 'org-timeblock-mark-block
      "%" 'org-timeblock-mark-by-regexp
      "u" 'org-timeblock-unmark-block
      "U" 'org-timeblock-unmark-all-blocks
      ;; Other actions
      "w" 'org-timeblock-write
      "+" 'org-timeblock-new-task
      "q" 'quit-window
      "ZZ" 'quit-window
      "ZQ" 'quit-window
      "C-s" 'org-save-all-org-buffers)

    ;; org-timeblock-list-mode Evil keybindings
    (evil-define-key 'normal org-timeblock-list-mode-map
      ;; Vim-like navigation
      "j" 'org-timeblock-list-next-line
      "k" 'org-timeblock-list-previous-line
      ;; Alternative navigation (original keybindings)
      "n" 'org-timeblock-list-next-line
      "p" 'org-timeblock-list-previous-line
      ;; Day navigation
      "f" 'org-timeblock-day-later
      "b" 'org-timeblock-day-earlier
      "C-f" 'org-timeblock-day-later
      "C-b" 'org-timeblock-day-earlier
      ;; Scrolling (Evil-like)
      "C-d" 'org-timeblock-day-later
      "C-u" 'org-timeblock-day-earlier
      ;; Entry actions
      (kbd "RET") 'org-timeblock-list-goto
      (kbd "TAB") 'org-timeblock-list-goto-other-window
      "o" 'org-timeblock-list-goto-other-window
      ;; Editing
      "d" 'org-timeblock-list-set-duration
      "i" 'org-timeblock-list-clock-in
      "O" 'org-clock-out
      ;; Refresh and navigation
      "gr" 'org-timeblock-redraw-buffers
      "gj" 'org-timeblock-jump-to-day
      "gg" 'org-timeblock-jump-to-day
      ;; Quit
      "q" 'org-timeblock-quit
      "ZZ" 'org-timeblock-quit
      "ZQ" 'org-timeblock-quit
      ;; Scheduling and todos
      "s" 'org-timeblock-list-schedule
      "T" 'org-timeblock-list-toggle-timeblock
      "t" 'org-timeblock-todo
      ;; View options
      "v" 'org-timeblock-switch-scaling
      "V" 'org-timeblock-change-span
      ;; Other actions
      "+" 'org-timeblock-new-task
      "C-s" 'org-save-all-org-buffers)))

;;;###autoload
(defun org-timeblock-evil-setup ()
  "Manually set up Evil keybindings for org-timeblock.
This function can be called by users who want to set up Evil keybindings
manually, or if the automatic setup doesn't work for some reason."
  (interactive)
  (org-timeblock-setup-evil-keybindings))

;; Set up Evil keybindings when Evil is loaded
(with-eval-after-load 'evil
  (org-timeblock-setup-evil-keybindings))

;; Also set up keybindings if Evil is already loaded
(when (featurep 'evil)
  (org-timeblock-setup-evil-keybindings))

(provide 'org-timeblock-evil)

;;; org-timeblock-evil.el ends here 