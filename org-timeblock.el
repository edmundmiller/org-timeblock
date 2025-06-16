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

(provide 'org-timeblock)

;;; org-timeblock.el ends here
