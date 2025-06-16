;;; org-timeblock-config.el --- Config for org-timeblock -*- lexical-binding: t; -*-

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

;; This file contains the configuration for org-timeblock, including
;; faces, custom variables, and other variables.

;;; Code:

;;;; Faces

;; The colors are borrowed from pulsar.el project written by Protesilaos Stavrou

(defface org-timeblock-red
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffcccc" :foreground "#77002a")
    (((class color) (min-colors 88) (background dark))
     :background "#77002a" :foreground "#ffcccc")
    (t :inverse-video t))
  "Red face."
  :group 'org-timeblock)

(defface org-timeblock-green
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#aceaac" :foreground "#00422a")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a" :foreground "#aceaac")
    (t :inverse-video t))
  "Green face."
  :group 'org-timeblock)

(defface org-timeblock-yellow
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#fff29a" :foreground "#693200")
    (((class color) (min-colors 88) (background dark))
     :background "#693200" :foreground "#fff29a")
    (t :inverse-video t))
  "Yellow face."
  :group 'org-timeblock)

(defface org-timeblock-blue
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8fcfff" :foreground "#242679")
    (((class color) (min-colors 88) (background dark))
     :background "#242679" :foreground "#8fcfff")
    (t :inverse-video t))
  "Blue face."
  :group 'org-timeblock)

(defface org-timeblock-magenta
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff" :foreground "#71206a")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a" :foreground "#ffccff")
    (t :inverse-video t))
  "Magenta face."
  :group 'org-timeblock)

(defface org-timeblock-cyan
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4" :foreground "#004065")
    (((class color) (min-colors 88) (background dark))
     :background "#004065" :foreground "#8eecf4")
    (t :inverse-video t))
  "Cyan face."
  :group 'org-timeblock)

(defface org-timeblock-list-header '((t (:inherit org-agenda-structure)))
  "Face used in org-timeblock-list for dates."
  :group 'org-timeblock)

(defface org-timeblock-select
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#ff6600")
    (((class color) (min-colors 88) (background dark))
     :background "#ff3300")
    (t :inverse-video t))
  "Face used for selected blocks."
  :group 'org-timeblock)

(defface org-timeblock-mark
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#7b435c")
    (((class color) (min-colors 88) (background dark))
     :background "#7b435c")
    (t :inverse-video t))
  "Face used for marked blocks."
  :group 'org-timeblock)

(defface org-timeblock-hours-line
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#7b435c")
    (((class color) (min-colors 88) (background dark))
     :background "#cdcdcd")
    (t :inverse-video t))
  "Face used for hour lines."
  :group 'org-timeblock)

(defface org-timeblock-current-time-indicator
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "red")
    (((class color) (min-colors 88) (background dark))
     :background "red")
    (t :inverse-video t))
  "Color face used for current time indicator."
  :group 'org-timeblock)

;;;; Custom Variables

(defgroup org-timeblock nil
  "Customization for `org-timeblock'."
  :group 'org
  :link '(url-link "https://github.com/ichernyshovvv/org-timeblock"))

(defcustom org-timeblock-show-future-repeats nil
  "Non-nil shows repeated entries in the future dates of repeat.
When set to the symbol `next' only the first future repeat is shown."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Show all repeated entries" t)
	  (const :tag "Show next repeated entry" next)
	  (const :tag "Do not show repeated entries" nil)))

(defcustom org-timeblock-files 'agenda
  "Org files with agenda items to display with `org-timeblock'.
When set to the symbol \\='agenda', `org-agenda-files' are used.
Otherwise, it may be set to a list of filenames."
  :group 'org-timeblock
  :type '(choice
	  (repeat :tag "List of files" file)
	  (const :tag "Files from (org-agenda-files)" agenda))
  :set (lambda (option value)
	 (set-default option value)
	 (setq org-timeblock-cache nil)
	 (setq org-timeblock-buffers nil)
	 (setq org-timeblock-markers nil)))

(defcustom org-timeblock-show-outline-path nil
  "Non-nil means show outline path in echo area for the selected item."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Don't show outline path with prepended file name." nil)
	  (const :tag "Show outline path." t)))

(defcustom org-timeblock-span 3
  "Number of days displayed in `org-timeblock'."
  :group 'org-timeblock
  :type 'integer)

(defcustom org-timeblock-display-time t
  "Non-nil means show end and start time inside timeblocks."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Show time." t)
	  (const :tag "Do not show time." nil)))

(defcustom org-timeblock-inbox-file
  (expand-file-name "inbox.org" org-directory)
  "Org file in which new tasks are created via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type 'file)

(defcustom org-timeblock-new-task-time
  'pick
  "Time to which new tasks are scheduled via `org-timeblock-new-task'."
  :group 'org-timeblock
  :type
  '(choice
    (const :tag "Unspecified.  The new task will be scheduled to a date with no time" nil)
    (const :tag "The new task will be scheduled to a time picked by user." pick)
    (string :tag "Time of the format \"HH:MM\".  The new task will be scheduled to a time.")))

(defcustom org-timeblock-scale-options t
  "Options that are used to decide which part of visual schedule must be hidden."
  :group 'org-timeblock
  :type '(choice
	  (const :tag "Hide hours in the past (if there are no timeblocks)." t)
	  (const :tag "Do not hide anything.  All 24 hours will be displayed." nil)
	  (const :tag "Hide all free hours before the first timeblock." hide-all)
	  (cons :tag "Display specified range of hours [earliest; latest)."
		(integer :tag "Min Hour")
		(integer :tag "Max Hour"))))

(defcustom org-timeblock-current-time-indicator t
  "Whether to show current time indicator in the `org-timeblock-list' buffer."
  :group 'org-timeblock
  :type 'boolean)

(defcustom org-timeblock-tag-colors
  nil
  "Faces for specific tags.

Alist of the form
  ((\"tag1\" . face-1)
   (\"tag2\" . face-2)).

In `org-timeblock-mode', timeblocks tagged with a tag in car are
painted in :background color of the face in cdr.  In
`org-timeblock-list-mode', faces are used to colorize items that
are tagged with a tag in car."
  :type 'list
  :group 'org-timeblock)

(defcustom org-timeblock-evil-keybindings t
  "Non-nil means enable Evil-mode keybindings in org-timeblock buffers.
When enabled, provides Vim-like navigation and editing keybindings
in both `org-timeblock-mode' and `org-timeblock-list-mode'."
  :group 'org-timeblock
  :type 'boolean)

;;;; Variables

(defvar org-timeblock-markers nil)

(defvar org-timeblock-buffers nil)

(defvar org-timeblock-mark-count 0)

(defvar org-timeblock-colors nil)

(defvar org-timeblock-data nil)
(defvar org-timeblock-column 1
  "Currently selected column.")

(defvar org-timeblock-selected-block-id nil
  "ID of the currently selected block.")

(defvar org-timeblock-selected-block-index 0
  "Index of the currently selected block within the current column.")

(defvar org-timeblock-cache nil)
(defvar org-timeblock-svg nil)
(defvar org-timeblock-svg-width 0)
(defvar org-timeblock-svg-height 0)

(defvar org-timeblock-daterange nil
  "The date range that is used to get and display schedule data.")

(defvar org-timeblock-duration-multipliers
  '((?w . 10080)
    (?d . 1440)
    (?h . 60)
    (?m . 1))
  "Duration multipliers used in `org-timeblock-read-duration'.")

(defvar org-timeblock-buffer
  "*org-timeblock*" "The name of the buffer displaying visual schedule.")

(defvar org-timeblock-list-buffer "*org-timeblock-list*"
  "The name of the buffer displaying the list of tasks and events.")


(provide 'org-timeblock-config)

;;; org-timeblock-config.el ends here 