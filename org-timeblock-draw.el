;;; org-timeblock-draw.el --- Drawing functions for org-timeblock -*- lexical-binding: t; -*-

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

;; This file contains functions for drawing the SVG calendar.

;;; Code:

(require 'org-timeblock-config)
(require 'org-timeblock-util)
(require 'org-timeblock-org)

(defun org-timeblock-redraw-timeblocks ()
  "Redraw *org-timeblock* buffer."
  (with-current-buffer (get-buffer-create org-timeblock-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if-let ((entries (org-timeblock-get-entries
			 (car org-timeblock-daterange)
			 (cdr org-timeblock-daterange)
			 t))
	       (dates (org-timeblock-get-dates
		       (car org-timeblock-daterange)
		       (cdr org-timeblock-daterange)))
	       (window (get-buffer-window org-timeblock-buffer))
	       ((setq org-timeblock-svg-height (window-body-height window t)
		      org-timeblock-svg-width (window-body-width window t))))
	  (let* ((column-width (/ org-timeblock-svg-width (length dates)))
		 (timeline-left-padding (* 2 (default-font-width)))
		 (block-max-width (- column-width timeline-left-padding))
		 (max-hour (if (consp org-timeblock-scale-options)
			       (if (= (cdr org-timeblock-scale-options) 24)
				   24
				 (1+ (cdr org-timeblock-scale-options)))
			     24))
		 (cur-time (decode-time)))
	    (setq org-timeblock-svg (svg-create org-timeblock-svg-width org-timeblock-svg-height))
	    (dotimes (iter (length dates))
	      (if-let ((entries
			(seq-filter
			 (lambda (x)
			   (let ((timestamp (org-timeblock-get-ts-prop x)))
			     (and
			      (org-timeblock--timestamp-relevant-p
			       timestamp (nth iter dates))
			      (or (not (consp org-timeblock-scale-options))
				  (<= (car org-timeblock-scale-options)
				      (org-element-property :hour-start timestamp)
				      (cdr org-timeblock-scale-options))
				  (and
				   (org-element-property :hour-end timestamp)
				   (or (<=
					(org-element-property :hour-start timestamp)
					(car org-timeblock-scale-options)
					(org-element-property :hour-end timestamp))
				       (<= (car org-timeblock-scale-options)
					   (org-element-property :hour-end timestamp)
					   (cdr org-timeblock-scale-options))))))))
			 entries)))
		  (let* ((order -1)
			 (min-hour
			  (pcase org-timeblock-scale-options
			    ((pred consp) (car org-timeblock-scale-options))
			    (`nil 0)
			    (_ (apply #'min (remove
					     nil
					     (append
					      (list (unless (eq org-timeblock-scale-options 'hide-all) (decoded-time-hour (decode-time))))
					      (mapcar
					       (lambda (entry)
						 (let ((s-or-e (org-timeblock-get-ts-prop entry)))
						   (if (and (org-timeblock-date< (org-timeblock-timestamp-to-time s-or-e) (nth iter dates))
							    (not (org-element-property :repeater-type s-or-e)))
						       0
						     (org-element-property :hour-start s-or-e))))
					       entries)))))))
			 (scale (/ org-timeblock-svg-height (float (* (- max-hour min-hour) 60))))
			 (cur-time-indicator
			  (and org-timeblock-current-time-indicator
			       (* scale
				  (-
				   (+ (* (decoded-time-hour cur-time) 60)
				      (decoded-time-minute cur-time)) ;; minutes
				   (* min-hour 60)))))
			 (columns
			  (mapcar (lambda (x) (cons (get-text-property 0 'id x) 1)) entries))
			 placed
			 (hour-lines-color
			  (face-attribute 'org-timeblock-hours-line :background nil t)))
		    (dolist (entry entries)
		      (let* ((timestamp (org-timeblock-get-ts-prop entry))
			     (repeated (org-element-property
					:repeater-type timestamp))
			     (start-ts (org-timeblock-timestamp-to-time
					timestamp))
			     (end-ts (org-timeblock-timestamp-to-time
				      timestamp t))
			     (start-date-earlier-p (org-timeblock-date<
						    start-ts (nth iter dates)))
			     (end-date-later-p (org-timeblock-date<
						(nth iter dates) end-ts)))
			(add-text-properties
			 0 (length entry)
			 `( time-string
			    ,(and
			      org-timeblock-display-time
			      (or repeated (not
					    (or end-date-later-p
						start-date-earlier-p)))
			      (concat
			       (org-timeblock-format-time " %H:%M" start-ts)
			       (and end-ts (org-timeblock-format-time "-%H:%M" end-ts))
			       (and repeated
				    (concat
				     " "
				     (pcase (org-element-property :repeater-type timestamp)
				       (`cumulate "+") (`catch-up "++") (`restart ".+"))
				     (let ((val (org-element-property :repeater-value timestamp)))
				       (and val (number-to-string val)))
				     (pcase (org-element-property :repeater-unit timestamp)
				       (`hour "h") (`day "d") (`week "w") (`month "m") (`year "y"))))))
			    block-height
			    ,(- (if (and start-ts end-ts)
				    (max
				     (default-font-height)
				     (round
				      (* (org-timeblock-time-diff
					  (if (or (and end-date-later-p (not repeated))
						  (org-timeblock-decoded<
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (1- max-hour)
						    :minute 59 :second 0)
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (decoded-time-hour end-ts)
						    :minute (decoded-time-minute end-ts))))
					      (org-timeblock-time-apply
					       (nth iter dates)
					       :hour (1- max-hour)
					       :minute 59
					       :second 0)
					    end-ts)
					  (if (or (and start-date-earlier-p (not repeated))
						  (org-timeblock-decoded<
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour (decoded-time-hour start-ts)
						    :minute (decoded-time-minute start-ts))
						   (org-timeblock-time-apply
						    (nth iter dates)
						    :hour min-hour :minute 0
						    :second 0)))
					      (org-timeblock-time-apply
					       (nth iter dates)
					       :hour min-hour :minute 0
					       :second 0)
					    start-ts))
					 scale)))
				  (default-font-height))
				(if (eq 'event (org-timeblock-get-ts-type entry)) 2 1))
			    y
			    ,(if-let ((value (+ (round (* (if (or (and start-date-earlier-p (not repeated))
								  (org-timeblock-decoded<
								   (org-timeblock-time-apply
								    (nth iter dates)
								    :hour (decoded-time-hour start-ts)
								    :minute (decoded-time-minute start-ts))
								   (org-timeblock-time-apply
								    (nth iter dates)
								    :hour min-hour :minute 0
								    :second 0)))
							      0
							    (- (+ (* 60 (org-element-property :hour-start timestamp))
								  (org-element-property :minute-start timestamp))
							       (* min-hour 60)))
							  scale))
						(if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				      ((< (- org-timeblock-svg-height value) (default-font-height))))
				 (- org-timeblock-svg-height (default-font-height))
			       value)
			    n-day-indicator
			    ,(and (not repeated)
				  (cond
				   ((and end-date-later-p start-date-earlier-p) "↕️")
				   (end-date-later-p "⬇️")
				   (start-date-earlier-p "⬆️"))))
			 entry)))
		    ;; Timeblocks layout algorithm
		    (dolist (entry entries)
		      (let ((id (get-text-property 0 'id entry)))
			(push entry placed)
			(setcdr (assoc id columns)
				(catch 'found-column
				  (let ((k 1))
				    (while t
				      (catch 'next-column
					(dolist (el (seq-filter
						     (lambda (x)
						       (eq (cdr (assoc (get-text-property 0 'id x) columns)) k))
						     placed))
					  (and (not (string= (get-text-property 0 'id el) id))
					       (org-timeblock-intersect-p entry el)
					       (cl-incf k)
					       (throw 'next-column t)))
					(throw 'found-column k))))))))
		    ;; Drawing hour lines
		    (let ((lines-iter (if (> min-hour 0) (1- min-hour) 0)) y)
		      (while (< (cl-incf lines-iter) max-hour)
			(setq y (round (* scale (- lines-iter min-hour) 60)))
			(svg-line
			 org-timeblock-svg
			 (+ timeline-left-padding (* column-width iter))
			 y
			 (+ column-width (* column-width iter))
			 y
			 :stroke-dasharray "4"
			 :stroke hour-lines-color)
			(svg-text
			 org-timeblock-svg (format "%d" lines-iter)
			 :y (+ y 5)
			 :x (* column-width iter)
			 :fill (face-attribute 'default :foreground))))
		    ;; Drawing current time indicator
		    (and cur-time-indicator
			 (org-timeblock-date= (nth iter dates) cur-time)
			 (svg-line
			  org-timeblock-svg
			  (* column-width iter)
			  cur-time-indicator
			  (+ column-width (* column-width iter))
			  cur-time-indicator
			  :stroke (face-attribute
				   'org-timeblock-current-time-indicator
				   :background nil t)))
		    ;; Drawing all the entries inside the timeline
		    (dolist (entry entries)
		      (when-let ((length
				  (1+ (length
				       (seq-uniq
					(mapcar
					 ;; get columns for those entries
					 (lambda (x)
					   (cdr (assoc (get-text-property 0 'id x) columns)))
					 ;; find those with which current entry is in intersection
					 (seq-filter
					  (lambda (x)
					    (unless
						(equal
						 (get-text-property 0 'id entry)
						 (get-text-property 0 'id x))
					      (org-timeblock-intersect-p entry x)))
					  entries))
					#'eq))))
				 (y (get-text-property 0 'y entry))
				 (block-height
				  (get-text-property 0 'block-height entry))
				 ((> (+ y block-height) 0))
				 (x (+ (+ timeline-left-padding
					  (round
					   (* (1- (cdr
						   (assoc
						    (get-text-property 0 'id entry)
						    columns)))
					      (/ block-max-width length))))
				       (* column-width iter)
				       (if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				 (block-width
				  (- (round (/ block-max-width length))
				     (if (eq 'event (org-timeblock-get-ts-type entry)) 2 1)))
				 (title
				  (concat (get-text-property 0 'title entry)
					  (get-text-property 0 'n-day-indicator entry)))
				 ;; Splitting the title of an entry
				 (heading-list
				  (if (> (* (length title) (default-font-width))
					 block-width)
				      (seq-take
				       (seq-partition title (/ block-width (default-font-width)))
				       (let ((lines-count
					      (round
					       (/ block-height (default-font-height)))))
					 (if (= 0 lines-count) 1 lines-count)))
				    `(,title))))
			(let ((time-string
			       (get-text-property 0 'time-string entry))
			      (face (org-timeblock-get-colors
				     (get-text-property 0 'tags entry))))
			  (when (< (/ block-width (default-font-width))
				   (length time-string))
			    (setq time-string nil))
			  (when-let ((time-string)
				     ((< (- block-height
					    (* (length heading-list) (default-font-height)))
					 (- (default-font-height) 6)))
				     (diff (-
					    (+ (length (car (last heading-list)))
					       (length time-string))
					    (/ block-width (default-font-width))))
				     ((> diff 0)))
			    (cl-callf
				(lambda (x)
				  (if (> (- (length x) diff) 10)
				      (substring x 0 (- diff))
				    (setq time-string nil)
				    x))
				(car (last heading-list))))
			  (push (list (get-text-property 0 'id entry)
				      (get-text-property 0 'marker entry))
				org-timeblock-data)
			  ;; Appending generated rectangle for current entry
			  (svg-rectangle
			   org-timeblock-svg x y block-width block-height
			   :column (1+ iter)
			   :stroke
			   (if (eq 'event (org-timeblock-get-ts-type entry))
			       "#5b0103" "#cdcdcd")
			   :stroke-width
			   (if (eq 'event (org-timeblock-get-ts-type entry))
			       2 1)
			   :opacity "0.7"
			   :order (cl-incf order)
			   :fill
			   (or
			    (and
			     (eq 'deadline (org-timeblock-get-ts-type entry))
			     "#5b0103")
			    (and face (face-attribute face :background nil 'default))
			    (face-attribute
			     (org-timeblock-get-saved-random-face title)
			     :background nil 'default))
			   ;; Same timestamp can be displayed in multiple
			   ;; columns, so _column-number postfix is used to tell
			   ;; that blocks apart
			   :id (format "%s_%d"
				       (get-text-property 0 'id entry)
				       (1+ iter))
			   :type (org-timeblock-get-ts-type entry))
			  ;; Setting the title of current entry
			  (let ((y (- y 5)))
			    (dolist (heading-part heading-list)
			      (svg-text org-timeblock-svg heading-part
					:x x
					:y (cl-incf y (default-font-height))
					:fill
					(or
					 (and
					  (eq 'deadline (org-timeblock-get-ts-type entry))
					  "#ffffff")
					 (and face (face-attribute face :foreground nil 'default))
					 (face-attribute
					  (org-timeblock-get-saved-random-face title)
					  :foreground nil 'default))
					:font-size
					(aref (font-info (face-font 'default)) 2))))
			  (when time-string
			    (svg-text org-timeblock-svg time-string
				      :x (- (+ x block-width)
					    (* (length time-string)
					       (default-font-width)))
				      :y (- (+ y block-height) 2)
				      :fill
				      (or
				       (and
					(eq 'deadline
					    (org-timeblock-get-ts-type entry))
					"#ffffff")
				       (and face (face-attribute face :foreground nil 'default))
				       hour-lines-color)
				      :font-size
				      (aref (font-info (face-font 'default)) 2)))))))
		(let ((message "No data."))
		  (svg-text org-timeblock-svg message
			    :y (/ org-timeblock-svg-height 2)
			    :x (+ (- (/ column-width 2)
				     (/ (* (default-font-width)
					   (length message))
					2))
				  (* column-width iter))
			    :fill (face-attribute 'default :foreground)
			    :font-size
			    (aref (font-info (face-font 'default)) 2)))))
	    (svg-insert-image org-timeblock-svg))
	(let* ((window (get-buffer-window org-timeblock-buffer))
	       (window-height (window-body-height window t))
	       (window-width (window-body-width window t))
	       (message "No data."))
	  (setq org-timeblock-svg (svg-create window-width window-height))
	  (svg-text
	   org-timeblock-svg message
	   :y (/ window-height 2)
	   :x (- (/ window-width 2)
		 (/ (* (default-font-width) (length message)) 2))
	   :fill (face-attribute 'default :foreground))
	  (svg-insert-image org-timeblock-svg)))
      (setq org-timeblock-mark-count 0)
      (org-timeblock-redisplay))))

(defun org-timeblock-redisplay ()
  "Redisplay *org-timeblock* buffer."
  (let ((inhibit-read-only t))
    (when-let ((window (get-buffer-window org-timeblock-buffer)))
      (if (or (< (window-body-height window t) org-timeblock-svg-height)
	      (< (window-body-width window t) org-timeblock-svg-width))
	  (org-timeblock-redraw-timeblocks)
	(setq header-line-format
	      (let* ((dates (org-timeblock-get-dates
			     (car org-timeblock-daterange)
			     (cdr org-timeblock-daterange)))
		     (left-fringe (/ (car (window-fringes window))
				     (default-font-width)))
		     (max-length (/ (+ (/ (window-body-width window t)
					  (default-font-width))
				       left-fringe)
				    (length dates)))
		     (date-format
		      (pcase max-length
			((pred (< 15)) "[%Y-%m-%d %a]")
			((pred (< 11)) "[%Y-%m-%d]")
			((pred (< 6)) "[%m-%d]")
			((pred (< 3)) "[%d]")))
		     (right-margin (format "%% -%ds" max-length))
		     (result (make-string left-fringe ? )))
		(dotimes (iter (length dates))
		  (cl-callf concat result
		    (propertize
		     (format right-margin
			     (org-timeblock-format-time date-format (nth iter dates)))
		     'face
		     (and (= org-timeblock-column (1+ iter))
			  'org-timeblock-select))))
		result))))
    (svg-possibly-update-image org-timeblock-svg)))

(defun org-timeblock-write (file)
  "Write the current *org-timeblock* buffer to FILE.

Depending on the extension of the file name, PNG image (.png),
SVG image (.svg), PDF (.pdf) is produced."
  (interactive "FWrite timeblocks to [PDF|SVG|PNG] file : \n")
  (unless (eq major-mode 'org-timeblock-mode)
    (user-error "Not in org-timeblock buffer"))
  (if (or (not (file-writable-p file))
	  (and (file-exists-p file)
	       (if (called-interactively-p 'any)
		   (not (y-or-n-p
			 (format "Overwrite existing file %s? " file))))))
      (user-error "Cannot write agenda to file %s" file))
  (org-timeblock-unselect-block)
  (let ((file (expand-file-name file))
	(svg (copy-sequence org-timeblock-svg)))
    ;; delete marker to not trigger `svg-possibly-update-image'
    (dom-remove-attribute svg :image)
    (with-temp-buffer
      (let* ((dates (org-timeblock-get-dates
		     (car org-timeblock-daterange)
		     (cdr org-timeblock-daterange)))
	     (max-length (/ (+ (/ org-timeblock-svg-width (default-font-width)))
			    (length dates)))
	     (date-format
	      (pcase max-length
		((pred (< 15)) "[%Y-%m-%d %a]")
		((pred (< 11)) "[%Y-%m-%d]")
		((pred (< 6)) "[%m-%d]")
		((pred (< 3)) "[%d]"))))
	(dom-add-child-before
	 svg
	 (dom-node
	  'rect
	  (list
	   (cons 'x 0)
	   (cons 'y 0)
	   (cons 'width org-timeblock-svg-width)
	   (cons 'height org-timeblock-svg-height)
	   (cons 'fill (face-attribute 'default :background)))))
	(dotimes (iter (length dates))
	  (svg-text
	   svg (org-timeblock-format-time date-format (nth iter dates))
	   :y org-timeblock-svg-height
	   :x (+ 5 (* (/ org-timeblock-svg-width (length dates)) iter))
	   :fill (face-attribute 'default :foreground)))
	(svg-print svg))
      (pcase (file-name-extension file)
	((or "pdf" "png")
	 (unless (executable-find "inkscape")
	   (user-error "Inkscape executable not found"))
	 (call-process-region (point-min) (point-max)
			      "inkscape" nil nil nil "--pipe"
			      (concat "--export-filename=" file)))
	((or "svg" `nil) (write-region nil nil file)))
      (org-timeblock-redraw-timeblocks))))

(provide 'org-timeblock-draw)

;;; org-timeblock-draw.el ends here 