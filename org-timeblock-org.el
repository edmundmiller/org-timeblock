;;; org-timeblock-org.el --- Org mode integration for org-timeblock -*- lexical-binding: t; -*-

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

;; This file contains functions for interacting with Org mode files,
;; parsing entries, and managing the cache.

;;; Code:

(require 'org-timeblock-config)
(require 'org-timeblock-util)

(defun org-timeblock-files ()
  "Get a list of files in which tasks are searched."
  (pcase org-timeblock-files
    (`agenda (org-agenda-files))
    ((and list (pred listp)) list)))

(defun org-timeblock--timestamp-relevant-p (timestamp date)
  "Check if org-element TIMESTAMP is relevant to DATE.
DATE is decoded-time value."
  (let ((start-ts (org-timeblock-timestamp-to-time timestamp)))
    (or
     (org-timeblock-date= start-ts date)
     (when-let ((org-timeblock-show-future-repeats)
		(value (org-element-property
			:repeater-value timestamp))
		(unit
		 (pcase (org-element-property
			 :repeater-unit timestamp)
		   (`week
		    (setq value (* value 7))
		    'day)
		   ((and _ u) u)))
		(start start-ts))
       (or
	(and (eq unit 'day)
	     (= value 1)
	     (if (eq org-timeblock-show-future-repeats 'next)
		 (org-timeblock-date= (org-timeblock-time-inc 'day 1 start) date)
	       (org-timeblock-date<= start date)))
	(progn
	  (if (eq org-timeblock-show-future-repeats 'next)
	      (setq start (org-timeblock-time-inc unit value start))
	    (while (org-timeblock-date< start date)
	      (setq start (org-timeblock-time-inc unit value start))))
	  (org-timeblock-date= start date))))
     (when-let ((end-ts (org-timeblock-timestamp-to-time timestamp t)))
       (and (org-timeblock-date< start-ts date)
	    (org-timeblock-date<= date end-ts))))))

(defun org-timeblock-get-buffer-entries-all (buffer)
  "Get all not done and not archived entries with any active timestamp in BUFFER."
  (let (entries tags
		update-markers-alist-p
		(buffer-markers
		 (alist-get buffer
			    org-timeblock-markers nil nil #'equal)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-tsr-regexp nil t)
	 (if (save-match-data
	       (or (org-entry-is-done-p)
		   (progn
		     (setq tags
			   (mapcar #'substring-no-properties
				   (org-get-tags)))
		     (member org-archive-tag tags))))
	     (org-get-next-sibling)
	   (when-let
	       ((timestamp-and-type
		 (save-excursion
		   (goto-char (match-beginning 0))
		   (list
		    (org-element-timestamp-parser)
		    (save-excursion
		      (cond
		       ((re-search-backward
			 (concat org-scheduled-regexp "[ \t]*\\=")
			 nil
			 t)
			'sched)
		       ((re-search-backward
			 (concat org-deadline-regexp "[ \t]*\\=")
			 nil
			 t)
			'deadline)
		       (t 'event)))
		    (or (seq-find (lambda (x) (= x (point))) buffer-markers)
			(let ((marker (copy-marker (point) t)))
			  (setq update-markers-alist-p t)
			  (push marker buffer-markers)
			  marker)))))
		(timestamp (car timestamp-and-type))
		(type (cadr timestamp-and-type))
		(marker (caddr timestamp-and-type))
		(start-ts (org-timeblock-timestamp-to-time
			   timestamp))
		(title (or (org-get-heading t nil t t) "no title")))
	     (save-excursion
	       (org-back-to-heading-or-point-min t)
	       (push
		(propertize
		 (concat
		  (org-timeblock--construct-prefix timestamp type)
		  title)
		 'type type
		 'timestamp timestamp
		 'marker marker
		 'tags tags
		 'id (org-timeblock-construct-id marker)
		 'title title)
		entries)))))))
    (when update-markers-alist-p
      (setf
       (alist-get buffer
		  org-timeblock-markers nil nil #'equal)
       buffer-markers))
    entries))

(defun org-timeblock-get-entries (from to &optional timeblocks)
  "Return scheduled tasks or events in [FROM;TO] timerange.
FROM and TO are decoded-time values.

When TIMEBLOCKS is non-nil, exclude entries with daterange or
without time."
  (org-timeblock-update-cache)
  (seq-filter
   (lambda (x)
     (when-let
	 ((timestamp (org-timeblock-get-ts-prop x))
	  ((or (not timeblocks)
	       (and
		(not (org-timeblock--daterangep timestamp))
		(org-element-property :hour-start timestamp))))
	  (start-ts (org-timeblock-timestamp-to-time
		     timestamp)))
       (or
	(and
	 (org-element-property
	  :repeater-type timestamp)
	 (org-timeblock-date<= start-ts from))
	(and
	 (org-timeblock-date<= from start-ts)
	 (org-timeblock-date<= start-ts to))
	(let ((end-ts
	       (org-timeblock-timestamp-to-time
		timestamp t)))
	  (and
	   end-ts
	   (org-timeblock-date<= from end-ts)
	   (org-timeblock-date<= end-ts to))))))
   org-timeblock-cache))

(defun org-timeblock-update-cache ()
  "Update org-timeblock-cache.
Return nil if buffers are up-to-date."
  (when-let
      ((buffers-to-update
	(mapcar
	 #'find-file-noselect
	 ;; This code is partially borrowed from `org-ql--select-cached'
	 ;; function which is part of org-ql project written by Adam Porter
	 (seq-filter
	  (lambda (file)
	    (let* ((buffer (find-file-noselect file))
		   (modified-tick
		    (alist-get file org-timeblock-buffers nil nil #'equal))
		   (new-modified-tick (buffer-chars-modified-tick buffer)))
	      (and (not (eq new-modified-tick modified-tick))
		   (setf
		    (alist-get file org-timeblock-buffers nil nil #'equal)
		    new-modified-tick))))
	  (org-timeblock-files)))))
    (setq org-timeblock-cache
	  (sort
	   (apply
	    #'append
	    (seq-remove
	     (lambda (x)
	       (member (marker-buffer (get-text-property 0 'marker x))
		       buffers-to-update))
	     org-timeblock-cache)
	    (mapcar
	     #'org-timeblock-get-buffer-entries-all
	     buffers-to-update))
	   #'org-timeblock-timestamp<))
    t))

(defun org-timeblock-get-colors (tags)
  "Return face for TAGS."
  (catch 'found
    (dolist (tag tags)
      (when-let ((colors (cdr (seq-find (lambda (x) (string= (car x) tag))
					org-timeblock-tag-colors))))
	(throw 'found colors)))))

(defun org-timeblock--construct-prefix (timestamp type)
  "Construct prefix for TIMESTAMP of type TYPE.

TIMESTAMP is org-element timestamp object which is used to
construct a timerange inside the prefix."
  (let ((hstart (org-element-property :hour-start timestamp))
	(mstart (org-element-property :minute-start timestamp))
	(hend (org-element-property :hour-end timestamp))
	(mend (org-element-property :minute-end timestamp))
	(type-str (pcase type
		    (`event "EVENT")
		    (`sched "SCHED")
		    (`deadline "DEADL"))))
    (propertize
     (format
      " %s % -12s % -6s "
      type-str
      (if (org-timeblock--daterangep timestamp)
	  ""
	(concat (and hstart mstart
		     (format
		      "%02d:%02d"
		      hstart
		      mstart))
		(and hend mend
		     (or (/= hend hstart)
			 (/= mend mstart))
		     (format
		      "-%02d:%02d"
		      hend
		      mend))))
      (concat
       ""
       (pcase (org-element-property :repeater-type timestamp)
	 (`cumulate "+") (`catch-up "++") (`restart ".+"))
       (when-let ((val (org-element-property :repeater-value timestamp)))
	 (number-to-string val))
       (pcase (org-element-property :repeater-unit timestamp)
	 (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
     'prefix t)))

(defun org-timeblock--schedule (start-ts &optional end-ts)
  "Schedule org entry at point.
START-TS and END-TS are Emacs decoded time values.

Return new org-element timestamp object."
  (let* ((planning-func
	  (cond ((save-excursion
		   (re-search-backward
		    (concat org-deadline-regexp "[ \t]*\\=")
		    nil
		    t))
		 #'org-deadline)
		((save-excursion
		   (re-search-backward
		    (concat org-scheduled-regexp "[ \t]*\\=")
		    nil
		    t))
		 #'org-schedule)))
	 (regexp-with-ts
	  (if (eq planning-func #'org-schedule)
	      org-scheduled-time-regexp
	    org-deadline-time-regexp))
	 (keyword-regexp
	  (concat
	   (if (eq planning-func #'org-schedule)
	       org-scheduled-regexp
	     org-deadline-regexp)
	   "[ \t]*")))
    (if planning-func
	(save-excursion
	  (let* ((timestamp (org-element-timestamp-parser))
		 (repeat-string (org-get-repeat
				 (org-element-property :raw-value timestamp)))
		 (warning-string
		  (concat
		   (pcase (org-element-property :warning-type timestamp)
		     (`first "--") (`all "-"))
		   (let ((val (org-element-property :warning-value timestamp)))
		     (and val (number-to-string val)))
		   (pcase (org-element-property :warning-unit timestamp)
		     (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))
		 (dates-equal-p (org-timeblock-date= start-ts end-ts)))
	    (cond
	     ((or (not end-ts) dates-equal-p)
	      (funcall-interactively
	       planning-func nil
	       (org-timeblock-ts-to-org-timerange start-ts end-ts)))
	     ((and end-ts (not dates-equal-p))
	      (funcall-interactively
	       planning-func nil
	       (org-timeblock-ts-to-org-timerange start-ts))
	      (when (re-search-forward
		     regexp-with-ts
		     (line-end-position) t)
		(insert "--"
			(org-timeblock-ts-to-org-timerange
			 end-ts
			 nil repeat-string warning-string)))))
	    (org-back-to-heading t)
	    (forward-line)
	    (re-search-forward keyword-regexp (line-end-position) t)
	    (org-element-timestamp-parser)))
      (when (or (looking-at org-tr-regexp)
		(looking-at org-ts-regexp))
	(replace-match
	 (org-timeblock-ts-to-org-timerange start-ts end-ts)))
      (org-element-timestamp-parser))))

(defun org-timeblock--duration (duration marker)
  "Set SCHEDULED duration to DURATION for the org entry at MARKER.
Change SCHEDULED timestamp duration of the org entry at MARKER.
Return the changed org-element timestamp object."
  (unless (marker-buffer marker)
    (user-error "Non-existent marker's buffer"))
  (org-with-point-at marker
    (org-timeblock-show-context)
    (let ((timestamp (org-element-timestamp-parser)))
      (unless (and (org-element-property :hour-start timestamp)
		   (org-element-property :minute-start timestamp))
	(user-error "The timestamp does not have time"))
      (let* ((start-ts (org-timeblock-timestamp-to-time timestamp))
	     (new-end-ts (org-timeblock-time-inc
			  'minute duration start-ts)))
	(org-timeblock--schedule start-ts new-end-ts)))))

(provide 'org-timeblock-org)

;;; org-timeblock-org.el ends here 