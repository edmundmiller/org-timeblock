;;; org-timeblock.el --- Interactive SVG calendar for orgmode tasks -*- lexical-binding: t; -*-

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

;; org-timeblock.el is a package that allows you to visually
;; understand your day schedule, quickly reschedule your tasks and set
;; TODO statuses

;;; Code:

;;;; Requirements

(require 'org)
(require 'svg)
(require 'seq)
(require 'compat)
(require 'compat-macs)

(require 'org-timeblock-config)
(require 'org-timeblock-util)
(require 'org-timeblock-org)
(require 'org-timeblock-draw)
(require 'org-timeblock-modes)

;;;; Interactive Commands

;;;###autoload
(defun org-timeblock-list ()
  "Enter `org-timeblock-list-mode'."
  (interactive)
  (switch-to-buffer org-timeblock-list-buffer)
  (setq org-timeblock-daterange
	(cons (decode-time)
	      (org-timeblock-time-inc 'day (1- org-timeblock-span)
				      (decode-time))))
  (org-timeblock-redraw-buffers))

;;;###autoload
(defun org-timeblock ()
  "Enter `org-timeblock-mode'."
  (interactive)
  (switch-to-buffer org-timeblock-buffer)
  (org-timeblock-mode)
  (org-timeblock-redraw-buffers))

;;;###autoload
(defun org-timeblock-clock-in (&optional select)
  "Start the clock on the currently selected block.
See `org-clock-in' to read about what tasks selection options
SELECT prefix argument provides."
  (interactive "P")
  (if (equal select '(4))
      (org-clock-in select)
    (when-let ((marker (org-timeblock-selected-block-marker)))
      (org-with-point-at marker
	(org-timeblock-show-context)
	(org-clock-in select)))))

;;;###autoload
(defun org-timeblock-list-clock-in (&optional select)
  "Start the clock on the item at point.
See `org-clock-in' to read about what tasks selection options
SELECT prefix argument provides."
  (interactive "P")
  (if (equal select '(4))
      (org-clock-in select)
    (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
      (org-with-point-at marker
	(org-timeblock-show-context)
	(org-clock-in select)))))

;;;###autoload
(cl-defun org-timeblock-new-task
    (&optional (date (pcase major-mode
		       (`org-timeblock-mode
			(nth (1- org-timeblock-column)
			     (org-timeblock-get-dates
			      (car org-timeblock-daterange)
			      (cdr org-timeblock-daterange))))
		       (`org-timeblock-list-mode
			(org-timeblock-list-get-current-date)))))
  "Create a task scheduled to DATE.
If DATE is nil, use the date in the current view.

The new task is created in `org-timeblock-inbox-file'"
  (interactive)
  (unless (member org-timeblock-inbox-file (org-timeblock-files))
    (user-error "`org-timeblock-inbox-file' must be present in `org-timeblock-files'"))
  (let ((title ""))
    (while (string-empty-p title)
      (setq title (read-string "Heading: ")))
    (with-current-buffer (find-file-noselect org-timeblock-inbox-file)
      (goto-char (point-max))
      (insert "\n")
      (org-insert-heading nil t t)
      (insert "TODO " title " ")
      (pcase org-timeblock-new-task-time
	(`pick (funcall-interactively #'org-schedule nil))
	((pred stringp)
	 (unless
	     (string-match-p "\\([01][0-9]\\|2[0-3]\\):[0-5][0-9]"
			     org-timeblock-new-task-time)
	   (user-error "Wrong time format specified in `org-timeblock-new-task-time'"))
	 (org-schedule nil (concat (org-timeblock-format-time "%Y-%m-%d " date)
				   org-timeblock-new-task-time)))
	(`nil (org-schedule nil (org-timeblock-format-time "%Y-%m-%d" date)))
	(_ (user-error "Invalid custom variable value")))
      (save-buffer)))
  (org-timeblock-redraw-buffers))

;;;;; Entry Actions

;;;###autoload
(defun org-timeblock-goto ()
  "Go to the entry of the selected block."
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-timeblock-show-context)))

;;;###autoload
(defun org-timeblock-goto-other-window ()
  "Go to the entry of the selected block in other window."
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char marker)
    (org-timeblock-show-context)))

;;;###autoload
(defun org-timeblock-schedule ()
  "Schedule the selected block to a different time."
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (org-with-point-at marker
      (org-timeblock-show-context)
      (call-interactively #'org-schedule))
    (org-timeblock-redraw-buffers)))

;;;###autoload
(defun org-timeblock-set-duration ()
  "Set the duration of the selected block."
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (let ((duration (org-timeblock-read-duration)))
      (org-timeblock--duration duration marker)
      (org-timeblock-redraw-buffers))))

;;;###autoload
(defun org-timeblock-todo ()
  "Change the TODO state of the selected block."
  (interactive)
  (when-let ((marker (org-timeblock-selected-block-marker)))
    (org-with-point-at marker
      (org-timeblock-show-context)
      (org-todo))
    (org-timeblock-redraw-buffers)))

;;;;; View Options

;;;###autoload
(defun org-timeblock-jump-to-day ()
  "Jump to a specific day."
  (interactive)
  (let* ((date (org-read-date nil t))
         (decoded-date (decode-time date)))
    (setq org-timeblock-daterange
          (cons decoded-date
                (org-timeblock-time-inc 'day (1- org-timeblock-span) decoded-date)))
    (setq org-timeblock-column 1)
    (org-timeblock-redraw-buffers)
    (message "Jumped to %s" (org-timeblock-format-time "%Y-%m-%d" decoded-date))))

;;;###autoload
(defun org-timeblock-switch-scaling ()
  "Switch between different scaling options."
  (interactive)
  (setq org-timeblock-scale-options
        (pcase org-timeblock-scale-options
          (`t nil)
          (`nil 'hide-all)
          (`hide-all t)))
  (org-timeblock-redraw-buffers)
  (message "Switched scaling to: %s" org-timeblock-scale-options))

;;;###autoload
(defun org-timeblock-change-span ()
  "Change the number of days displayed."
  (interactive)
  (let ((new-span (read-number "Number of days to display: " org-timeblock-span)))
    (setq org-timeblock-span new-span)
    (setq org-timeblock-daterange
          (cons (car org-timeblock-daterange)
                (org-timeblock-time-inc 'day (1- new-span) (car org-timeblock-daterange))))
    (org-timeblock-redraw-buffers)
    (message "Changed span to %d days" new-span)))

;;;;; Marking Functions

;;;###autoload
(defun org-timeblock-mark-block ()
  "Mark the currently selected block."
  (interactive)
  (message "Block marking not yet implemented"))

;;;###autoload
(defun org-timeblock-mark-by-regexp ()
  "Mark blocks by regexp."
  (interactive)
  (message "Regexp marking not yet implemented"))

;;;###autoload
(defun org-timeblock-unmark-block ()
  "Unmark the currently selected block."
  (interactive)
  (message "Block unmarking not yet implemented"))

;;;###autoload
(defun org-timeblock-unmark-all-blocks ()
  "Unmark all blocks."
  (interactive)
  (message "Unmarking all blocks not yet implemented"))

;;;;; List Mode Functions

;;;###autoload
(defun org-timeblock-list-goto ()
  "Go to the entry at point in list mode."
  (interactive)
  (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-timeblock-show-context)))

;;;###autoload
(defun org-timeblock-list-goto-other-window ()
  "Go to the entry at point in other window in list mode."
  (interactive)
  (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char marker)
    (org-timeblock-show-context)))

;;;###autoload
(defun org-timeblock-list-set-duration ()
  "Set duration for the entry at point in list mode."
  (interactive)
  (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
    (let ((duration (org-timeblock-read-duration)))
      (org-timeblock--duration duration marker)
      (org-timeblock-redraw-buffers))))

;;;###autoload
(defun org-timeblock-list-schedule ()
  "Schedule the entry at point in list mode."
  (interactive)
  (when-let ((marker (get-text-property (line-beginning-position) 'marker)))
    (org-with-point-at marker
      (org-timeblock-show-context)
      (call-interactively #'org-schedule))
    (org-timeblock-redraw-buffers)))

;;;###autoload
(defun org-timeblock-list-get-current-date ()
  "Get the current date in list mode."
  (car org-timeblock-daterange))

;;;###autoload
(defun org-timeblock-quit ()
  "Quit org-timeblock list mode."
  (interactive)
  (quit-window))

;;;###autoload
(defun org-timeblock-toggle-timeblock-list ()
  "Toggle between timeblock and list views."
  (interactive)
  (if (eq major-mode 'org-timeblock-mode)
      (org-timeblock-list)
    (org-timeblock)))

;;;###autoload
(defun org-timeblock-list-toggle-timeblock ()
  "Toggle from list view to timeblock view."
  (interactive)
  (org-timeblock))

(provide 'org-timeblock)

;;; org-timeblock.el ends here
