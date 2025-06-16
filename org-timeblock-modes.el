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

;; This file contains the mode definitions and keymaps for org-timeblock.

;;; Code:

(eval-when-compile
  (require 'org-timeblock-config)
  (require 'org-timeblock-util)
  (require 'org-timeblock-org))

(require 'org-timeblock-config)
(require 'org-timeblock-util)
(require 'org-timeblock-org)

(declare-function org-timeblock-redisplay "org-timeblock-draw")
(declare-function org-timeblock-redraw-buffers "org-timeblock-draw")
(declare-function org-timeblock-write "org-timeblock-draw")
(declare-function org-timeblock-select-block-with-cursor "org-timeblock-draw")

;; Declare functions from main org-timeblock.el
(declare-function org-timeblock-goto "org-timeblock")
(declare-function org-timeblock-goto-other-window "org-timeblock")
(declare-function org-timeblock-schedule "org-timeblock")
(declare-function org-timeblock-set-duration "org-timeblock")
(declare-function org-timeblock-todo "org-timeblock")
(declare-function org-timeblock-jump-to-day "org-timeblock")
(declare-function org-timeblock-switch-scaling "org-timeblock")
(declare-function org-timeblock-change-span "org-timeblock")
(declare-function org-timeblock-mark-block "org-timeblock")
(declare-function org-timeblock-mark-by-regexp "org-timeblock")
(declare-function org-timeblock-unmark-block "org-timeblock")
(declare-function org-timeblock-unmark-all-blocks "org-timeblock")
(declare-function org-timeblock-list-goto "org-timeblock")
(declare-function org-timeblock-list-goto-other-window "org-timeblock")
(declare-function org-timeblock-list-set-duration "org-timeblock")
(declare-function org-timeblock-list-schedule "org-timeblock")
(declare-function org-timeblock-quit "org-timeblock")
(declare-function org-timeblock-toggle-timeblock-list "org-timeblock")
(declare-function org-timeblock-list-toggle-timeblock "org-timeblock")
(declare-function org-timeblock-new-task "org-timeblock")
(declare-function org-timeblock-clock-in "org-timeblock")
(declare-function org-timeblock-list-clock-in "org-timeblock")

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
   buffer-read-only t
   org-timeblock-column 1)
  ;; Initialize selection to first block in first column
  (org-timeblock-select-first-block-in-column)
  (org-timeblock-redisplay))

;;;###autoload
(defun org-timeblock-list-mode-set-highlights ()
  "Set font-lock highlights for `org-timeblock-list-mode'."
  (font-lock-add-keywords
   'org-timeblock-list-mode
   '(("^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" 1 font-lock-string-face t)
     ("[0-9]\\{2\\}:[0-9]\\{2\\}" 0 font-lock-function-name-face t))))

(define-derived-mode org-timeblock-list-mode
  special-mode "Org-Timeblock-List" :interactive nil
  (setq truncate-lines t)
  (org-timeblock-list-mode-set-highlights))

(with-eval-after-load 'evil
  (require 'org-timeblock-evil))

;;;;; Navigation Functions

;;;###autoload
(defun org-timeblock-forward-block ()
  "Move to the next block in the current column."
  (interactive)
  (when (eq major-mode 'org-timeblock-mode)
    (let* ((dates (org-timeblock-get-dates (car org-timeblock-daterange) (cdr org-timeblock-daterange)))
           (current-date (nth (1- org-timeblock-column) dates))
           (entries (org-timeblock-get-entries (car org-timeblock-daterange) (cdr org-timeblock-daterange) t))
           (current-entries (seq-filter 
                            (lambda (entry)
                              (when-let ((ts (org-timeblock-get-ts-prop entry)))
                                (org-timeblock-date= current-date (org-timeblock-timestamp-to-time ts))))
                            entries))
           (sorted-entries (sort (copy-sequence current-entries) #'org-timeblock-timestamp<)))
      (if sorted-entries
          (let* ((current-index (if org-timeblock-selected-block-id
                                   (or (seq-position sorted-entries 
                                                    (seq-find (lambda (e) 
                                                               (string= (get-text-property 0 'id e) 
                                                                       org-timeblock-selected-block-id))
                                                             sorted-entries)
                                                    #'equal)
                                       0)
                                 org-timeblock-selected-block-index))
                 (next-index (if (< current-index (1- (length sorted-entries)))
                                (1+ current-index)
                              0)) ; Wrap to first block
                 (next-entry (nth next-index sorted-entries)))
            (setq org-timeblock-selected-block-id (get-text-property 0 'id next-entry)
                  org-timeblock-selected-block-index next-index)
            (org-timeblock-redraw-buffers)
            (message "Selected block: %s" (get-text-property 0 'title next-entry)))
        (message "No blocks in current column")))))

;;;###autoload
(defun org-timeblock-backward-block ()
  "Move to the previous block in the current column."
  (interactive)
  (when (eq major-mode 'org-timeblock-mode)
    (let* ((dates (org-timeblock-get-dates (car org-timeblock-daterange) (cdr org-timeblock-daterange)))
           (current-date (nth (1- org-timeblock-column) dates))
           (entries (org-timeblock-get-entries (car org-timeblock-daterange) (cdr org-timeblock-daterange) t))
           (current-entries (seq-filter 
                            (lambda (entry)
                              (when-let ((ts (org-timeblock-get-ts-prop entry)))
                                (org-timeblock-date= current-date (org-timeblock-timestamp-to-time ts))))
                            entries))
           (sorted-entries (sort (copy-sequence current-entries) #'org-timeblock-timestamp<)))
      (if sorted-entries
          (let* ((current-index (if org-timeblock-selected-block-id
                                   (or (seq-position sorted-entries 
                                                    (seq-find (lambda (e) 
                                                               (string= (get-text-property 0 'id e) 
                                                                       org-timeblock-selected-block-id))
                                                             sorted-entries)
                                                    #'equal)
                                       0)
                                 org-timeblock-selected-block-index))
                 (prev-index (if (> current-index 0)
                                (1- current-index)
                              (1- (length sorted-entries)))) ; Wrap to last block
                 (prev-entry (nth prev-index sorted-entries)))
            (setq org-timeblock-selected-block-id (get-text-property 0 'id prev-entry)
                  org-timeblock-selected-block-index prev-index)
            (org-timeblock-redraw-buffers)
            (message "Selected block: %s" (get-text-property 0 'title prev-entry)))
        (message "No blocks in current column")))))

;;;###autoload
(defun org-timeblock-forward-column ()
  "Move to the next column (day)."
  (interactive)
  (when (eq major-mode 'org-timeblock-mode)
    (let ((dates (org-timeblock-get-dates (car org-timeblock-daterange) (cdr org-timeblock-daterange))))
      (when (< org-timeblock-column (length dates))
        (setq org-timeblock-column (1+ org-timeblock-column))
        ;; Select first block in the new column
        (org-timeblock-select-first-block-in-column)
        (org-timeblock-redisplay)
        (message "Moved to column %d" org-timeblock-column)))))

;;;###autoload
(defun org-timeblock-backward-column ()
  "Move to the previous column (day)."
  (interactive)
  (when (eq major-mode 'org-timeblock-mode)
    (when (> org-timeblock-column 1)
      (setq org-timeblock-column (1- org-timeblock-column))
      ;; Select first block in the new column
      (org-timeblock-select-first-block-in-column)
      (org-timeblock-redisplay)
              (message "Moved to column %d" org-timeblock-column))))

(defun org-timeblock-select-first-block-in-column ()
  "Select the first block in the current column."
  (let* ((dates (org-timeblock-get-dates (car org-timeblock-daterange) (cdr org-timeblock-daterange)))
         (current-date (nth (1- org-timeblock-column) dates))
         (entries (org-timeblock-get-entries (car org-timeblock-daterange) (cdr org-timeblock-daterange) t))
         (current-entries (seq-filter 
                          (lambda (entry)
                            (when-let ((ts (org-timeblock-get-ts-prop entry)))
                              (org-timeblock-date= current-date (org-timeblock-timestamp-to-time ts))))
                          entries))
         (sorted-entries (sort (copy-sequence current-entries) #'org-timeblock-timestamp<)))
    (if sorted-entries
        (let ((first-entry (car sorted-entries)))
          (setq org-timeblock-selected-block-id (get-text-property 0 'id first-entry)
                org-timeblock-selected-block-index 0))
      (setq org-timeblock-selected-block-id nil
            org-timeblock-selected-block-index 0))))

;;;###autoload
(defun org-timeblock-day-later ()
  "Move the date range one day later."
  (interactive)
  (setq org-timeblock-daterange
        (cons (org-timeblock-time-inc 'day 1 (car org-timeblock-daterange))
              (org-timeblock-time-inc 'day 1 (cdr org-timeblock-daterange))))
  ;; Reset selection to first block in current column
  (org-timeblock-select-first-block-in-column)
  (org-timeblock-redraw-buffers)
  (message "Moved to later date range"))

;;;###autoload
(defun org-timeblock-day-earlier ()
  "Move the date range one day earlier."
  (interactive)
  (setq org-timeblock-daterange
        (cons (org-timeblock-time-inc 'day -1 (car org-timeblock-daterange))
              (org-timeblock-time-inc 'day -1 (cdr org-timeblock-daterange))))
  ;; Reset selection to first block in current column
  (org-timeblock-select-first-block-in-column)
  (org-timeblock-redraw-buffers)
  (message "Moved to earlier date range"))

;;;###autoload
(defun org-timeblock-list-next-line ()
  "Move to the next line in list mode."
  (interactive)
  (when (eq major-mode 'org-timeblock-list-mode)
    (forward-line 1)))

;;;###autoload
(defun org-timeblock-list-previous-line ()
  "Move to the previous line in list mode."
  (interactive)
  (when (eq major-mode 'org-timeblock-list-mode)
    (forward-line -1)))

(provide 'org-timeblock-modes)

;;; org-timeblock-modes.el ends here
