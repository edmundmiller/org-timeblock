;;; org-timeblock-util.el --- Utility functions for org-timeblock -*- lexical-binding: t; -*-

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

;; This file contains utility functions for org-timeblock.

;;; Code:

(require 'org-timeblock-config)

(compat-version "29.1")

(compat-defun org-fold-show-context (&optional key)
  "Make sure point and context are visible."
  (org-show-context key))

(defun org-timeblock-show-context ()
  "Make sure point and context are visible."
  (compat-call org-fold-show-context 'agenda))

(defsubst org-timeblock-format-time (format-string time)
  "Use FORMAT-STRING to format the time value TIME."
  (let ((time (copy-sequence time)))
    (unless (decoded-time-second time)
      (setf (decoded-time-second time) 0))
    (unless (decoded-time-minute time)
      (setf (decoded-time-minute time) 0))
    (unless (decoded-time-hour time)
      (setf (decoded-time-hour time) 0))
    (format-time-string format-string (encode-time time))))

(cl-defsubst org-timeblock-get-ts-prop (&optional object (position 0))
  "Return POSITION's \\='timestamp property, in OBJECT."
  (get-text-property position 'timestamp object))

(cl-defsubst org-timeblock-get-ts-type (&optional object (position 0))
  "Return POSITION's \\='type property, in OBJECT."
  (get-text-property position 'type object))

(defun org-timeblock-cursor-pos ()
  "Return cursor position in the window of the *org-timeblock* buffer.
If cursor position is outside of the window, return nil.

Cursor position is of the form (X . Y)."
  (when-let ((cursor-pos (cdr (mouse-pixel-position)))
	     (window (get-buffer-window org-timeblock-buffer))
	     (pos (window-edges window t nil t)))
    (when (and (> (- (car cursor-pos) (car pos)) 0)
	       (> (- (cdr cursor-pos) (cadr pos)) 0))
      (cons (- (car cursor-pos) (car pos))
	    (- (cdr cursor-pos) (cadr pos))))))

(defun org-timeblock-get-marker-by-id (id)
  "Return a marker of entry with ID."
  (cadr (seq-find (lambda (x) (string= (car x) id)) org-timeblock-data)))

(defun org-timeblock-get-dates (from to)
  "Return a list of decoded-time dates between FROM and TO."
  (let (dates)
    (while (and
	    (push from dates)
	    (setq from (org-timeblock-time-inc 'day 1 from))
	    (org-timeblock-date<= from to)))
    (nreverse dates)))

(defmacro org-timeblock-on (accessor op lhs rhs)
  "Run OP on ACCESSOR's return values from LHS and RHS."
  `(,op (,accessor ,lhs) (,accessor ,rhs)))

(defun org-timeblock-date= (a b)
  "Return non-nil if dates of A and B time values are equal."
  (cond
   ((and (null a) (null b)))
   ((and a b)
    (and (org-timeblock-on decoded-time-year  = a b)
         (org-timeblock-on decoded-time-month = a b)
         (org-timeblock-on decoded-time-day   = a b)))))

(defun org-timeblock-date< (a b)
  "Return non-nil if A's date is less than B's date."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-year < a b)
	(and
	 (org-timeblock-on decoded-time-year = a b)
	 (or (org-timeblock-on decoded-time-month < a b)
	     (and (org-timeblock-on decoded-time-month = a b)
		  (org-timeblock-on decoded-time-day < a b))))))))

(defun org-timeblock-time-diff (a b)
  "Return difference between times A and B in minutes."
  (when-let ((a (encode-time a))
	     (b (encode-time b)))
    (/ (time-convert (time-subtract a b) 'integer) 60)))

(defun org-timeblock-decoded< (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (time-less-p
     (encode-time a)
     (encode-time b)))))

(defun org-timeblock-decoded= (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (time-equal-p
     (encode-time a)
     (encode-time b)))))

(defun org-timeblock-time< (a b)
  "Return non-nil if A's time is earlier then B's time.
Compare only hours and minutes."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-hour < a b)
	(and
	 (org-timeblock-on decoded-time-hour = a b)
	 (org-timeblock-on decoded-time-minute < a b))))))

(defun org-timeblock-date<= (a b)
  "Return non-nil if A's date is <= B's date."
  (cond
   ;; nil is less than non-nil
   ((null b) nil)
   ((null a) t)
   (t
    (or (org-timeblock-on decoded-time-year < a b)
	(and
	 (org-timeblock-on decoded-time-year = a b)
	 (or (org-timeblock-on decoded-time-month < a b)
	     (and (org-timeblock-on decoded-time-month = a b)
		  (org-timeblock-on decoded-time-day <= a b))))))))

(defsubst org-timeblock-get-ts (item)
  "Return ITEM's \\='timestamp text property as decoded time."
  (org-timeblock-timestamp-to-time (org-timeblock-get-ts-prop item)))

(defsubst org-timeblock-get-saved-random-face (title)
  "Get saved random color face for TITLE.
If not found, generate it with `org-timeblock--random-color',
save it and return."
  (or (alist-get title org-timeblock-colors nil nil #'equal)
      (setf (alist-get title org-timeblock-colors nil nil #'equal)
	    (org-timeblock--random-color))))

(defun org-timeblock-timestamp< (a b)
  "Return t, if A's \\='timestamp is less then B's."
  (org-timeblock-on org-timeblock-get-ts org-timeblock-time< a b))

(defun org-timeblock-intersect-p (entry1 entry2)
  "Return t, if two entries intersect each other.
Otherwise, return nil.
`ENTRY1',`ENTRY2' - strings returned from `org-timeblock-get-entries'."
  (when-let ((y1 (get-text-property 0 'y entry1))
	     (y2 (get-text-property 0 'y entry2)))
    (let ((y1-end (+ (get-text-property 0 'block-height entry1) y1))
	  (y2-end (+ (get-text-property 0 'block-height entry2) y2)))
      (or
       (= y2 y1)
       (and
	y2-end
	(< y2 y1)
	(< y1 y2-end))
       (and
	y1-end
	(< y1 y2)
	(< y2 y1-end))))))

(defun org-timeblock--random-color ()
  "Return random org-timeblock color face."
  (seq-random-elt
   '(org-timeblock-red
     org-timeblock-green
     org-timeblock-yellow
     org-timeblock-blue
     org-timeblock-magenta
     org-timeblock-cyan)))

(defun org-timeblock--daterangep (timestamp)
  "Return t if org timestamp object TIMESTAMP is a daterange with no time."
  (when-let ((day-end (org-element-property :day-end timestamp))
	     (month-end (org-element-property :month-end timestamp))
	     (year-end (org-element-property :year-end timestamp)))
    (and
     (or
      (/= (org-element-property :day-start timestamp) day-end)
      (/= (org-element-property :month-start timestamp) month-end)
      (/= (org-element-property :year-start timestamp) year-end))
     (null (org-element-property :hour-start timestamp))
     (null (org-element-property :hour-end timestamp)))))

(cl-defun org-timeblock-read-ts (ts &optional (prompt "TIME:"))
  "Read a time in \"HHMM\" format and apply it to TS.
Return the changed time struct.

PROMPT can overwrite the default prompt."
  (let (time)
    (catch 'exit
      (while t
	(let ((len (length time))
	      (ch (read-char-exclusive
		   (concat "[format: HHMM] " prompt (reverse time)))))
	  (cond
	   ((or (and (= len 0) (<= ?0 ch ?2))
		(and (= len 1)
		     (if (< (car time) ?2) (<= ?0 ch ?9) (<= ?0 ch ?3)))
		(and (= len 2) (<= ?0 ch ?5)))
	    (push ch time))
	   ((and (= len 3) (<= ?0 ch ?9))
	    (push ch time)
	    (throw 'exit t))
	   ((and (/= len 0) (eq ch ?\C-?))
	    (pop time))
	   (t (ding))))))
    (cl-macrolet ((pop-digit () '(- (pop time) 48)))
      (org-timeblock-time-apply
       ts
       :minute (+ (pop-digit) (* 10 (pop-digit)))
       :hour (+ (pop-digit) (* 10 (pop-digit)))))))

(defun org-timeblock-construct-id (&optional marker)
  "Construct identifier for the timestamp at MARKER.
If MARKER is nil, use timestamp at point."
  (org-with-point-at (or marker (point))
    (md5
     (format
      "%s%d%s"
      (buffer-file-name (buffer-base-buffer))
      (point)
      (when (or (looking-at org-tr-regexp)
		(looking-at org-ts-regexp))
	(match-string 0)))
     nil nil 'utf-8)))

(defun org-timeblock-timestamp-to-time (ts &optional end)
  "Convert TS into an Emacs decoded time value.
If END is non-nil, use end part of the timestamp.

TS is a org-element timestamp object."
  (let ((year-start (org-element-property :year-start ts))
	(month-start (org-element-property :month-start ts))
	(day-start (org-element-property :day-start ts))
	(hour-start (org-element-property :hour-start ts))
	(minute-start (org-element-property :minute-start ts)))
    (if end
	(when-let ((year-end (org-element-property :year-end ts))
		   (month-end (org-element-property :month-end ts))
		   (day-end (org-element-property :day-end ts)))
	  (let ((hour-end (org-element-property :hour-end ts))
		(minute-end (org-element-property :minute-end ts)))
	    (when (or
		   (/= day-start day-end)
		   (/= month-start month-end)
		   (/= year-start year-end)
		   (and hour-end hour-start (/= hour-start hour-end))
		   (and minute-end minute-start (/= minute-start minute-end)))
	      (make-decoded-time
	       :year year-end :month month-end :day day-end
	       :hour (or hour-end 0) :minute (or minute-end 0) :second 0))))
      (make-decoded-time
       :year year-start :month month-start :day day-start
       :hour (or hour-start 0) :minute (or minute-start 0) :second 0))))

(defun org-timeblock-ts-to-org-timerange
    (ts-start &optional ts-end repeat-string warning-string)
  "Create an Org timestamp range string.

TS-START and TS-END are decoded time values.
REPEAT-STRING is a repeater string.
WARNING-STRING is a warning string of the form \"-[0-9]+[hdwmy]\""
  (when-let ((start-date (org-timeblock-format-time "%Y-%m-%d %a" ts-start)))
    (let ((start-time
	   (and (decoded-time-hour ts-start)
		(decoded-time-minute ts-start)
		(org-timeblock-format-time "%R" ts-start)))
	  (end-date (and ts-end (org-timeblock-format-time
				 "%Y-%m-%d %a" ts-end)))
	  (end-time (and ts-end
			 (and (decoded-time-hour ts-end)
			      (decoded-time-minute ts-end)
			      (org-timeblock-format-time "%R" ts-end))))
	  (timestamp-end
           (concat
            (and (org-string-nw-p repeat-string) (concat " " repeat-string))
            (and (org-string-nw-p warning-string) (concat " " warning-string))
            ">")))
      (concat
       "<" start-date (and start-time (concat " " start-time))
       (if (equal end-date start-date)
	   (and end-time (not (equal end-time start-time))
		(concat "-" end-time))
	 (and
	  end-date
	  (concat
	   timestamp-end
	   "--<" end-date
	   (and end-time (concat " " end-time)))))
       timestamp-end))))

(defun org-timeblock-read-duration ()
  "Read time duration and return minutes as an integer.

Beep or flash the screen when an invalid character is typed.  The
prompt shows currently valid characters for the next input
character.

Valid duration formats:
2h
2h30m
2h30
45
1d
1w3h30m"
  (let* ((dur "")
	 (all-multipliers (mapcar #'car org-timeblock-duration-multipliers))
	 (valid-multipliers all-multipliers)
	 typed-multipliers)
    (catch 'dur
      (while-let ((multipliers
		   (apply #'propertize
			  (concat "[" valid-multipliers "]")
			  (and (or (length= dur 0)
				   (member (string-to-char (substring dur -1))
					   all-multipliers))
			       '(face org-agenda-dimmed-todo-face))))
		  (ch (read-char-exclusive
		       (concat "DURATION ([0-9]+" multipliers "):" dur))))
	(cond
	 ((<= ?0 ch ?9)
	  (setq dur (format "%s%c" dur ch)))
	 ((or (and (eq ch ?\C-m) (length> dur 0))
	      (and (member ch valid-multipliers)
		   (string-match-p "[0-9]+$" dur)))
	  (when-let (((member ch '(?m ?\C-m)))
		     (minutes 0)
		     (start 0))
	    (setq dur (concat dur "m"))
	    (while (string-match
		    (concat "\\([0-9]+\\)\\([" typed-multipliers "m]\\)")
		    dur start)
	      (cl-incf minutes (* (cdr
				   (assq (string-to-char (match-string 2 dur))
					 org-timeblock-duration-multipliers))
				  (string-to-number (match-string 1 dur))))
	      (setq start (match-end 0)))
	    (throw 'dur minutes))
	  (setq dur (format "%s%c" dur ch)
		valid-multipliers (cdr (member ch valid-multipliers)))
	  (push ch typed-multipliers))
	 ((and (eq ?\C-? ch) (not (length= dur 0)))
	  (when (eq (string-to-char (substring dur -1)) (car typed-multipliers))
	    (pop typed-multipliers)
	    (setq valid-multipliers
		  (let ((ms all-multipliers))
		    (when typed-multipliers
		      (while (not (eq (pop ms) (car typed-multipliers)))))
		    ms)))
	  (setq dur (substring dur 0 -1)))
	 (t (ding)))))))

(defun org-timeblock-time-inc (slot value time)
  "Return a new time object based on TIME with its SLOT incremented by VALUE.

SLOT should be specified as a plain symbol, not a keyword."
  (let ((time (copy-sequence time)))
    (decoded-time-add
     time
     (make-decoded-time (intern (format ":%s" slot)) value))))

(cl-defun org-timeblock-time-apply (time &key second minute hour
					 day month year)
  "Return new timestamp based on TIME with new slot values from keys."
  ;; This code is borrowed from `ts-apply' function which is part of ts.el
  ;; project written by Adam Porter
  (let ((time (copy-sequence time)))
    (when second
      (setf (decoded-time-second time) second))
    (when minute
      (setf (decoded-time-minute time) minute))
    (when hour
      (setf (decoded-time-hour time) hour))
    (when day
      (setf (decoded-time-day time) day))
    (when month
      (setf (decoded-time-month time) month))
    (when year
      (setf (decoded-time-year time) year))
    time))

(defun org-timeblock-read-number-in-range (min max)
  "Read a number in [MIN; MAX] and return it."
  (cl-loop as n = (read-number
		   (format "Number [%d; %d]: " min max))
	   until (<= min n max)
	   finally return n))

(provide 'org-timeblock-util)

;;; org-timeblock-util.el ends here 