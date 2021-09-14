;;; blist.el --- Display bookmarks in an ibuffer way  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  李俊緯

;; Author: 李俊緯 <mmemmew@gmail.com>
;; Keywords: convenience, languages

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

;; This package uses the package `ilist' to display bookmarks in a
;; similar fashion to ibuffer.  The core functionalities are provided
;; by `ilist'.  This pacakge solely provides interactive functions
;; that are built upon `ilist'.

;;; Code:

;;; Dependencies

(require 'bookmark) ;; of course
(require 'ilist)    ;; core engine

;;; Customization options

;;;; Group

(defgroup blist ()
  "Display bookmarks in an Ibuffer manner."
  :group 'bookmark)

;;;; Display location or not

(defcustom blist-display-location-p t
  "Whether or not display the locations of bookmarks."
  :group 'blist
  :type 'boolean
  :local t)

;;;; No confirm deletion

(defcustom blist-expert nil
  "If non-nil, don't ask to confirm the deletion of bookmarks."
  :group 'blist
  :type 'boolean)

;;;; Filter groups

(defcustom blist-filter-groups (list
                                (cons "Eshell" #'blist-eshell-p)
                                (cons "Default" #'blist-default-p))
  "The filter groups for display.
Remember to put the default group at the end, otherwise some
bookmarks might not be displayed."
  :group 'blist
  :type '(repeat (cons string function)))

;;;; Empty groups

(defcustom blist-discard-empty-p t
  "Whether to display empty groups or not."
  :group 'blist
  :type 'boolean)

;;;; Cycle movement

(defcustom blist-movement-cycle t
  "Whether \"round\" the buffer when moving.
To round the buffer means to assume that the top of the buffer is
identified with the bottom of the buffer."
  :group 'blist
  :type 'boolean)

;;;; Maximal displayed bookmark name length

(defcustom blist-maximal-name-len 0.4
  "The maximal length of the displayed bookmark name."
  :group 'blist
  :type '(choice
          (integer :tag "Number of characters")
          (float :tag "Fraction of window body width")))

;;;; Eliding string

(defcustom blist-elide-string "..."
  "The string to put at the end of a string that is too long."
  :group 'blist
  :type 'string)

;;;; Deletion mark character

(defcustom blist-deletion-mark ?D
  "The character to mark a bookmark for deletion."
  :group 'blist
  :type 'character)

;;;; Default mark character

(defcustom blist-default-mark ?>
  "The character that is used for mark by default."
  :group 'blist
  :type 'character)

;;;; Sorter

(defcustom blist-default-sorter nil
  "The default sorter.
See `blist-sorter'."
  :group 'blist
  :type '(choice
          (const nil :tag "No sorting")
          (function :tag "Sorting function")))

;;;; Annotation Column Name

(defcustom blist-annotation-column-name "A"
  "The name of the column showing whether a bookmark has \
annotations.

Only the first letter will be shown.

If one changes this, run `blist-set-annotation-column' to set the
annotation column again."
  :group 'blist
  :type 'string
  :set #'blist-set-annotation-column)

;;;; How to open multiple bookmarks?

(defcustom blist-select-manner (list 'vertical)
  "How to select multiple bookmarks.
The value should be a list of symbols.  Allowed symbols are as
follows.

- vertical:        open bookmarks vertically.
- horizontal:      open bookmarks horizontally.  Vertical takes
                   precedence.
- spiral:          open bookmark in a spiral manner.  If vertical
                   is also present, spiral vertically; otherwise
                   spiral horizontally.  By default toward right
                   / down, unless left / up is present.
- main-side:       open bookmarks with the first in a main
                   window, and the other windows are put in side
                   splits.  If vertical, then the side splits are
                   either at the bottom or at the top; if
                   horizontal, then the sides are at the left or
                   the right.  This overrides spiral.
- left:            the direction to open.  The bookmarks that are
                   higher on the list are opened on the right.
- right, up, down: similar to left.  Left takes precedence over
                   right, and up takes precedence over down.
- tab:             open the bookmarks in a new tab.  Requires
                   'tab-bar if used.

As a quick shortcut, if the list does not contain left, it means
to use right; if no up, it means down; if no vertical, it means
horizontal.

There will be no errors if there are unrecognized symbols in the
list; they are simply ignored."
  :group 'blist
  :type '(repeat
          (choice
           (const :tag "Vertically" vertical)
           (const :tag "Horizontally" horizontal)
           (const :tag "Spirally" spiral)
           (const :tag "A main window along with side splits" main-side)
           (const :tag "Towards Left" left)
           (const :tag "Towards Right" right)
           (const :tag "Towards Up" up)
           (const :tag "Towards Down" down)
           (const :tag "In a new tab" tab))))

;;; Variables

;;;; Sorter

(defvar blist-sorter blist-default-sorter
  "The function used to sort the bookmark list.
See `ilist-string' for how the sorter should behave.")

;;;; Rename history

(defvar blist-rename-history nil
  "The variable that stores the strings that the user has inputted \
to rename bookmarks.")

;;; Display

;;;; Columns

;;;;; Name Column

(defun blist-name-column ()
  "Return the column specification for NAME."
  (declare (pure t) (side-effect-free t))
  (list
   "Name" #'bookmark-name-from-full-record
   5 (cond
      ((integerp blist-maximal-name-len)
       blist-maximal-name-len)
      ((and (floatp blist-maximal-name-len)
            (< 0 blist-maximal-name-len 1))
       (floor
        (* (window-body-width) blist-maximal-name-len)))
      ((user-error "`blist-maximal-name-len' should be either \
an integer or a float between 0 and 1")))
   :left
   "..."))

;;;;; Annotation column

(defun blist-get-annotation (bookmark)
  "Return if BOOKMARK has annotation.
If BOOKMARK has no annotation, return a space string."
  (cond
   ((let ((annotation (bookmark-get-annotation bookmark)))
      (and (stringp annotation)
           (not (string= annotation ""))))
    "*")
   (" ")))

(defvar blist-annotation-column
  (list blist-annotation-column-name #'blist-get-annotation
        1 1 :left nil)
  "The specification of the ANNOTATION column.")

(defun blist-set-annotation-column (&rest _args)
  "Set the annotation column.
ARGS are there to conform to the customization interface."
  (setq blist-annotation-column
        (list blist-annotation-column-name #'blist-get-annotation
              1 1 :left nil)))

;;;;; Location column

(defun blist-get-location (bookmark)
  "Return the location of BOOKMARK.
The location of a bookmark is either its filename, or its
location, in case it is not associated with a file."
  (declare (pure t) (side-effect-free t))
  (or (bookmark-get-filename bookmark)
      (bookmark-prop-get bookmark 'location)))

(defvar blist-location-column
  (list "Location" #'blist-get-location
        9 nil :left nil)
  "The specification for the LOCATION column.")

;;;; Groups

;;;;; Convenient macro

;; The user is expected to define groups through this macro.

(defmacro blist-define-criterion (name doc-name &rest forms)
  "Define a function to determine whether a bookmark is of the \
required type.

NAME is part of the name of the resulting function; specifically,
the resulting function is named blist-NAME-p.

Its documentation string uses DOC-NAME to describe its effect.

FORMS are ELisp forms that should return non-nil when BOOKMARK is
of the required type."
  (declare (indent 2))
  (append
   (list
    'defun (intern (format "blist-%s-p" name))
    (list 'bookmark)
    (format "Determine whether BOOKMARK is of type %s." doc-name)
    (list 'declare (list 'pure t) (list 'side-effect-free t)))
   forms))

;;;;; Eshell group

;; As an example, a group is defined by default

(blist-define-criterion "eshell" "Eshell"
  (eq (bookmark-get-handler bookmark) #'eshell-bookmark-jump))

;;;;; Default group

(blist-define-criterion "default" "DEFAULT" (ignore bookmark) t)

;;;; List function

;; REVIEW: Maybe I shall call `delete-trailing-whitespace' at the end
;; of the function to avoid surprises of the trailing whitespaces?

;;;###autoload
(defun blist-list-bookmarks (&rest _args)
  "List bookmarks in an ibuffer fashion.
The bookmarks are taken from `bookmark-alist'.

The ARGS is there so as to accept arguments in order for it to be
used as a `revert-buffer-function'."
  ;; load the bookmark if needed
  (bookmark-maybe-load-default-file)
  (let ((buffer (get-buffer-create bookmark-bmenu-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            ;; a final newline is important
            (delete-trailing-lines nil))
        (widen)
        (delete-region (point-min) (point-max))
        (insert
         (ilist-string
          bookmark-alist
          (append
           (list ilist-mark-column
                 blist-annotation-column
                 (blist-name-column))
           (cond
            (blist-display-location-p (list blist-location-column))))
          blist-filter-groups
          blist-discard-empty-p
          blist-sorter))
        (insert (string #xa))
        (delete-trailing-whitespace))
      (goto-char (point-min))
      (blist-mode))
    (display-buffer buffer)
    (select-window (get-buffer-window bookmark-bmenu-buffer))
    (ilist-forward-line 1 nil t)))

;;; Major mode

(define-derived-mode blist-mode ilist-mode "BList"
  "Major mode for interacting with the bookmarks displayed by \
`blist-list-bookmarks'."
  :group 'blist
  (setq-local revert-buffer-function #'blist-list-bookmarks))

;;;; Key-bindings

;; TODO: Sorting

(let ((map blist-mode-map))
  (define-key map (vector ?n) #'blist-next-line)
  (define-key map (vector #x20) #'next-line)
  (define-key map (vector ?p) #'blist-prev-line)
  (define-key map (vector ?N) #'blist-next-item)
  (define-key map (vector ?P) #'blist-prev-item)
  (define-key map (vector ?\M-n) #'blist-next-group)
  (define-key map (vector ?\M-p) #'blist-prev-group)
  (define-key map (vector 'tab) #'blist-next-group)
  (define-key map (vector 'backtab) #'blist-prev-group)
  (define-key map (vector 'return) #'blist-return)
  (define-key map (vector ?o) #'blist-open-other-window)
  (define-key map (vector ?v) #'blist-select)
  (define-key map (vector ?r) #'blist-rename)
  (define-key map (vector ?w) #'blist-locate)
  (define-key map (vector ?R) #'blist-relocate)
  (define-key map (vector ?a) #'blist-show-annotation)
  (define-key map (vector ?A) #'blist-show-all-annotations)
  (define-key map (vector ?e) #'blist-edit-annotation)
  (define-key map (vector ?/) #'blist-search)
  (define-key map (vector ?s) #'blist-save)
  (define-key map (vector #x29) #'blist-next-marked)
  (define-key map (vector #x28) #'blist-prev-marked)
  (define-key map (vector ?\M-}) #'blist-next-marked)
  (define-key map (vector ?\M-{) #'blist-prev-marked)
  (define-key map (vector ?m) #'blist-mark)
  (define-key map (vector ?d) #'blist-mark-for-deletion)
  (define-key map (vector ?\C-d) #'blist-mark-for-deletion-backward)
  (define-key map (vector ?k) #'blist-mark-for-deletion)
  (define-key map (vector ?D) #'blist-delete-marked)
  (define-key map (vector ?x) #'blist-do-delete)
  (define-key map (vector ?u) #'blist-unmark-forward)
  (define-key map (vector 'backspace) #'blist-unmark-backward)
  (define-key map (vector 'M-backspace) #'blist-unmark-all-mark)
  (define-key map (vector ?U) #'blist-unmark-all)
  (define-key map (vector ?t) #'blist-toggle-marks)
  (define-key map (vector ?T) #'blist-toggle-location)
  (define-key map (vector ?* ?*) #'blist-unmark-all-mark)
  (define-key map (vector ?* ?c) #'blist-change-marks)
  (define-key map (vector ?% ?n) #'blist-mark-by-name)
  (define-key map (vector ?% ?l) #'blist-mark-by-location)
  (define-key map (vector ?j) #'blist-jump-to-line)
  (define-key map (vector ?J) #'blist-jump-to-group)
  (define-key map (vector ?\M-j) #'blist-jump-to-group)
  (define-key map (vector ?\M-g) #'blist-jump-to-line)
  (define-key map (vector ?\M-G) #'blist-jump-to-group))

;;; Major mode functions

;;;; Assert

(defun blist-assert-mode ()
  "Ensure we are in `blist-mode' derived buffer."
  (cond
   ((derived-mode-p 'blist-mode))
   ((user-error "Not in `blist-mode'"))))

;;;; The range of operation

(defun blist-operate-range (arg &optional use-default-p default-start default-end)
  "Return the range for the operation.
If region is active, use the region.

If point is on a group header, use ARG groups.

Otherwise use ARG lines.

If USE-DEFAULT-P is non-nil, use DEFAULT-START and DEFAULT-END
instead of using ARG lines in the last case.

Return a cons cell (start . end) of start and end of the range.

Note that this function moves point to the other end of the
range, unless region is active."
  (let (start end no-sort-p)
    (cond
     ((use-region-p)
      (setq start (region-beginning))
      (setq end (region-end)))
     ((get-text-property (point) 'ilist-group-header)
      (setq start (point))
      (setq end (progn
                  (ilist-forward-group-header arg)
                  (point))))
     (use-default-p
      (setq no-sort-p t)
      (setq start default-start)
      (setq end default-end))
     ((setq start (point))
      (setq end (progn
                  (ilist-forward-line arg nil t)
                  (point)))))
    (cond
     (no-sort-p (cons start end))
     ((cons (min start end) (max start end))))))

;;;; Open bookmarks

;; I almost forgot that I want to open bookmarks.

;;;###autoload
(defun blist-open ()
  "Open the bookmark at point."
  (interactive)
  (blist-assert-mode)
  (cond
   ((ilist-get-index)
    (bookmark-jump
     (nth (ilist-get-index) bookmark-alist)))
   ((user-error "No bookmark to open on this line"))))

;;;; Open in another window

;;;###autoload
(defun blist-open-other-window ()
  "Open the bookmark at point in another window."
  (interactive)
  (blist-assert-mode)
  (cond ((ilist-get-index)
         (bookmark--jump-via
          (nth (ilist-get-index) bookmark-alist)
          #'switch-to-buffer-other-window))
        ((user-error "No bookmark to open on this line"))))

;;;; blist-select

;;;;; Prepare windows

(defun blist-prepare-select-windows (num)
  "Create and return NUM windows according to \
`blist-select-manner'.

NUM should be a positive integer."
  (cond
   ((or (not (integerp num))
        (<= num 0))
    (error "NUM should be a positive integer, but got %S" num)))
  (let* ((mainp (memq 'main-side blist-select-manner))
         (tabp (cond (mainp nil) ((memq 'tab blist-select-manner))))
         (verticalp (memq 'vertical blist-select-manner))
         (leftp (memq 'left blist-select-manner))
         (upp (memq 'up blist-select-manner))
         (spiralp (memq 'spiral blist-select-manner))
         (size (cond
                ;; spirals split in half
                (spiralp nil)
                ((and mainp (> num 1) verticalp)
                 (floor (frame-width) (1- num)))
                ((and mainp (> num 1))
                 (floor (frame-height) (1- num)))
                (mainp nil)
                (verticalp (floor (frame-height) num))
                ((floor (frame-width) num))))
         (current-direction verticalp)
         (orig-window (selected-window))
         temp-window windows main-side-splitted-p)
    (cond (tabp (require 'tab-bar) (tab-bar-new-tab)))
    ;; create a new window so that we are not inside some window that
    ;; cannot be splitted, like a side window
    (select-window (split-window (frame-root-window) nil 'below))
    (delete-other-windows)
    (setq orig-window (selected-window))
    (setq windows (cons orig-window windows))
    (setq temp-window orig-window)
    (setq num (1- num))
    (while (> num 0)
      (setq
       temp-window
       (split-window temp-window
                     (cond
                      ((and mainp (not main-side-splitted-p))
                       nil)
                      (size))
                     (cond
                      (current-direction
                       ;; vertical
                       (cond (upp 'above) ('below)))
                      ;; horizontal
                      (leftp 'left)
                      ('right))))
      (setq windows (cons temp-window windows))
      ;; change direction for spirals and change direction only once
      ;; for main-side
      (cond ((or spiralp
                 (and mainp (not main-side-splitted-p)))
             (setq current-direction (not current-direction))
             (setq main-side-splitted-p t)))
      (setq num (1- num)))
    (reverse windows)))

;;;;; select function

;;;###autoload
(defun blist-select ()
  "Open all marked bookmarks.
If there are no marked bookmarks, and if the point is on a group
header, open all bookmarks of the group.

If there are no marked bookmarks, and if the point is on a
bookmark line, then open the bookmark on that line.

Otherwise signal an error.

How the bookmarks are opened are controlled by the variable
`blist-select-manner'."
  (interactive)
  (blist-assert-mode)
  (let* ((marked-items (ilist-map-lines #'ilist-get-index
                                        #'ilist-is-marked))
         (marked-items
          (cond
           (marked-items)
           ((ilist-get-group)
            (let ((start (point))
                  (end (save-excursion
                         (ilist-forward-group-header 1)
                         (point))))
              (ilist-map-lines
               #'ilist-get-index #'ilist-get-index start end)))
           ;; HACK: if not on a normal line, it will return nil, so
           ;; that cond will skip this clause
           ((delq nil (list (ilist-get-index))))
           ((user-error "No bookmarks to open"))))
         (marked-items (mapcar
                        (lambda (index) (nth index bookmark-alist))
                        marked-items))
         (windows (blist-prepare-select-windows
                   (length marked-items)))
         (orig-window (car windows)))
    (while (consp windows)
      (select-window (car windows))
      (bookmark-jump (car marked-items))
      (setq marked-items (cdr marked-items))
      (setq windows (cdr windows)))
    (select-window orig-window)))

;;;; rename

;;;###autoload
(defun blist-rename (old new)
  "Rename the bookmark at point by NEW.
If the current buffer is in `blist-mode', this also runs
`revert-buffer' afterwards.

If called with \\[universal-argument], or if there is no bookmark
at point, prompt for the OLD bookmark to rename."
  (interactive
   (list
    (cond
     ((or current-prefix-arg
          (null (ilist-get-index)))
      (let* ((names (mapcar
                     #'bookmark-name-from-full-record
                     bookmark-alist))
             (default (cond
                       ((ilist-get-index)
                        (bookmark-name-from-full-record
                         (nth (ilist-get-index)
                              bookmark-alist)))))
             (prompt (cond
                      (default (format "Rename bookmark (%s): "
                                       default))
                      ("Rename bookmark: "))))
        (completing-read prompt names nil t nil
                         'blist-rename-history default)))
     ((bookmark-name-from-full-record
       (nth (ilist-get-index) bookmark-alist))))
    (read-string "Rename bookmark to: " nil 'blist-rename-history
                 (cond
                  ((ilist-get-index)
                   (bookmark-name-from-full-record
                    (nth (ilist-get-index) bookmark-alist)))))))
  ;; error out for invalid input
  (cond
   ((or (not (and
              (stringp old)
              (not (string= old ""))))
        (not (and
              (stringp new)
              (not (string= new "")))))
    (error "OLD and NEW should be non-empty strings, \
but got %S and %S"
           old new)))
  (bookmark-set-name old new)
  (cond ((derived-mode-p 'blist-mode) (revert-buffer)))
  ;; increase modification count
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  ;; save if the user wants to
  (cond ((bookmark-time-to-save-p) (bookmark-save))))

;;;; locate

;;;###autoload
(defun blist-locate ()
  "Display the location of the bookmark at point in the echo area."
  (interactive)
  (blist-assert-mode)
  (cond
   ((and (ilist-get-index)
         (blist-get-location
          (nth (ilist-get-index) bookmark-alist)))
    (message (blist-get-location
              (nth (ilist-get-index) bookmark-alist))))
   ((user-error "Unknown location"))))

;;;; relocate

(defvar blist-relocate-history nil
  "The history variable of `blist-relocate'.")

;;;###autoload
(defun blist-relocate (bookmark)
  "Relocate BOOKMARK to another location.

If the BOOKMARK has a attribute called relocater, call the value
of the attribute, which should be a function with one argument:
the BOOKMARK itself, to ask for the new location.  Custom
bookmark records can set this attribute to offer customized
relocating behaviour.

If not, use `read-file-name' to specify the new file name to
point to.

Otherwise, signal an error.

If called with \\[universal-argument], or if point is not at a
bookmark, use `completing-read' to let the user choose which
bookmark to relocate.

Otherwise, if point is at a bookmark, relocate that bookmark."
  (interactive
   (list
    (let* ((default (cond
                     ((ilist-get-index)
                      (bookmark-name-from-full-record
                       (nth (ilist-get-index) bookmark-alist)))))
           (prompt (cond (default
                           (format "Bookmark to relocate [%s]: "
                                   default))
                         ("Bookmark to relocate: "))))
      (cond
       (current-prefix-arg
        (completing-read
         prompt
         ;; copied from `bookmark-relocate'
         (lambda (str pred action)
           (if (eq action 'metadata)
               '(metadata (category . bookmark))
             (complete-with-action action bookmark-alist str pred)))
         nil t nil 'blist-relocate-history default))
       (default)
       ((user-error "No bookmark to relocate"))))))
  (blist-assert-mode)
  ;; paranoid
  (bookmark-maybe-load-default-file)
  (let* ((file-name (bookmark-get-filename bookmark))
         (location (bookmark-prop-get bookmark 'location))
         (file-or-location (or file-name location))
         (relocater (bookmark-prop-get bookmark 'relocater))
         (prompt (format "Relocate %s to: " bookmark))
         (new-location
          (cond
           ((functionp relocater)
            (funcall relocater bookmark))
           ((read-file-name
             prompt (file-name-directory file-or-location)
             file-or-location)))))
    (cond
     (file-name (bookmark-set-filename boomark new-location))
     (location (bookmark-prop-set bookmark 'location new-location)))
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (cond ((bookmark-time-to-save-p) (bookmark-save)))
    (cond ((derived-mode-p 'blist-mode) (revert-buffer)))))

;;;; show annotations

;;;###autoload
(defun blist-show-annotation ()
  "Show the annotation of the bookmark(s) in another window.
If there are marked bookmarks, show the annotations of the marked
bookmarks; otherwise show the annotations of the bookmark at
point.  If there is no bookmark at point, use `completing-read'
to choose one."
  (interactive)
  (blist-assert-mode)
  (let* ((marked-items
          (mapcar
           (lambda (index)
             (bookmark-name-from-full-record
              (nth index bookmark-alist)))
           (ilist-map-lines
            #'ilist-get-index #'ilist-is-marked)))
         (targets
          (cond
           (marked-items)
           ((mapcar
             (lambda (index)
               (bookmark-name-from-full-record
                (nth index bookmark-alist)))
             (delq nil (list (ilist-get-index)))))
           ((let ((items (mapcar
                          (lambda (index)
                            (bookmark-name-from-full-record
                             (nth index bookmark-alist)))
                          (blist-all-bookmarks))))
              (list
               (completing-read
                "Choose a bookmark to show annotation: "
                (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . bookmark))
                    (complete-with-action
                     action items str pred))))))))))
    (blist-show-annotations targets)))

;;;; show all annotations

;;;###autoload
(defun blist-show-all-annotations (targets)
  "Show the annotation of all bookmarks of TARGETS in another \
window."
  (interactive (list (mapcar
                      (lambda (index)
                        (bookmark-name-from-full-record
                         (nth index bookmark-alist)))
                      (blist-all-bookmarks))))
  (blist-assert-mode)
  (save-selected-window
    (pop-to-buffer (get-buffer-create "*Bookmark Annotation*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mapc
       (lambda (bookmark)
         ;; make sure we are dealing with records
         (let* ((bookmark (bookmark-get-bookmark bookmark))
                (name (bookmark-name-from-full-record
                       bookmark))
                (anno (bookmark-get-annotation bookmark))
                (anno (cond ((and anno (stringp anno)
                                  (not (string= anno "")))
                             (concat
                              (mapconcat
                               (lambda (str)
                                 (concat (make-string 4 #x20) str))
                               (split-string (format "%s\n" anno))
                               (string #xa))
                              "\n"))
                            (""))))
           (insert (format "%s:\n%s" name anno))))
       targets))
    (goto-char (point-min))
    (special-mode)))

;;;; edit annotations

;;;; search (limit)

;;;; save

;;;; load

;;;; blist-toggle-group

;;;###autoload
(defun blist-toggle-group ()
  "Toggle the visibility of the group at point."
  (interactive)
  (let ((group-header (ilist-get-group))
        (hidden-text (get-text-property (point) 'blist-hidden-text))
        (inhibit-read-only t))
    (cond
     ((null group-header)
      ;; not at group
      (user-error "Not at a group to toggle"))
     (hidden-text
      ;; hidden group
      (delete-region (line-beginning-position)
                     (min (1+ (line-end-position)) (point-max)))
      (save-excursion (insert hidden-text)))
     ;; not hidden
     ((let* ((start (line-beginning-position))
             (end (progn
                    (ilist-forward-group-header 1)
                    (point)))
             (text (buffer-substring start end)))
        (delete-region start end)
        (insert
         (propertize
          (format "[ %s ... ]\n" group-header)
          'ilist-group-header group-header
          'blist-hidden-text text))
        (goto-char start))))))

;;;; Generic return

;;;###autoload
(defun blist-return ()
  "Either open the bookmark or toggle the group."
  (interactive)
  (cond
   ((ilist-get-group) (blist-toggle-group))
   ((ilist-get-index) (blist-open))
   ((user-error "Nothing to do here"))))

;;;; Toggle location display

;;;###autoload
(defun blist-toggle-location ()
  "Toggle the display of locations of bookmarks."
  (interactive)
  (let (temp)
    ;; a little C-like hacky style
    (setq
     temp
     (setq-local blist-display-location-p
                 (not blist-display-location-p)))
    (blist-list-bookmarks)
    ;; restore the value of the variable
    (setq-local blist-display-location-p temp)))

;;;; All bookmarks, including hidden ones

(defun blist-all-bookmarks ()
  "Return the list of all bookmark indices, even the hidden ones."
  (append
   ;; normal lines
   (ilist-map-lines #'ilist-get-index #'ilist-get-index)
   ;; hidden lines
   (apply
    #'append
    (ilist-map-lines
     (lambda ()
       (blist-toggle-group)
       (let ((start (point))
             (end (save-excursion
                    (ilist-forward-group-header 1)
                    (point)))
             temp)
         (setq
          temp
          (ilist-map-lines #'ilist-get-index #'ilist-get-index
                           start end))
         (blist-toggle-group)
         temp))
     (lambda ()
       (get-text-property (point) 'blist-hidden-text))))))

;;;; Jumping around

;;;###autoload
(defun blist-jump-to-line (name)
  "Jump to the line containing the bookmark with NAME."
  (interactive
   (list
    (completing-read
     "Jump to bookmark: "
     (mapcar
      (lambda (index)
        (bookmark-name-from-full-record
         (nth index bookmark-alist)))
      (blist-all-bookmarks))
     nil t)))
  (blist-assert-mode)
  (let (res pos)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (ilist-boundary-buffer-p t))
                  (not res))
        (cond
         ;; a normal line
         ((ilist-get-index)
          (setq
           res
           (string=
            (bookmark-name-from-full-record
             (nth (ilist-get-index) bookmark-alist))
            name))
          (cond (res (setq pos (point))))))
        (ilist-forward-line 1)))
    ;; per chance the line is hidden

    ;; NOTE: if pos is already found, res will be non-nil, then the
    ;; following block will be automatically skipped.
    (save-excursion
      (goto-char (point-min))
      (while (and (not (ilist-boundary-buffer-p t))
                  (not res))
        (cond
         ;; a hidden line
         ((and (ilist-get-group)
               (get-text-property (point) 'blist-hidden-text))
          (let ((start (point))
                (end (save-excursion
                       (ilist-forward-group-header 1)
                       (point))))
            (blist-toggle-group)
            (ilist-forward-line 1)
            (while (and (not (ilist-get-group))
                        (not (ilist-boundary-buffer-p t))
                        (not res))
              (setq
               res
               (string=
                (bookmark-name-from-full-record
                 (nth (ilist-get-index) bookmark-alist))
                name))
              (cond (res (setq pos (point))))
              (ilist-forward-line 1))
            ;; if not found, we toggle back
            (cond
             ((not res)
              (goto-char start)
              (blist-toggle-group))))))
        ;; we skip a group ahead.
        (cond
         ((not res)
          (ilist-forward-group-header 1)))))
    (cond
     (pos (goto-char pos))
     ((user-error "No bookmark named %s" name)))))

;;;; blist-jump-to-group

;;;###autoload
(defun blist-jump-to-group (name)
    "Jump to the line containing the bookmark with NAME."
  (interactive
   (list
    (completing-read
     "Jump to group: "
     (ilist-map-lines #'ilist-get-group #'ilist-get-group)
     nil t)))
  (blist-assert-mode)
  (let (res pos)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (ilist-boundary-buffer-p t))
                  (not res))
        (cond
         ((ilist-get-group)
          (setq
           res
           (string=
            (ilist-get-group)
            name))
          (cond (res (setq pos (point))))))
        (ilist-forward-group-header 1)))
    (cond
     (pos (goto-char pos))
     ((user-error "No group named %s" name)))))

;;;; toggle marks

;;;###autoload
(defun blist-toggle-marks ()
   "Toggle the mark statuses in the buffer.
Lines marked with `blist-default-mark' become unmarked, and lines
not marked become marked with `blist-default-mark'.

If region is active, operate only on the region.

If the point is on a group header, then only operate on that group."
  (interactive)
  (let* ((inhibit-read-only t)
         (temp (save-excursion (blist-operate-range 1 t)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda ()
       (let* ((marks (ilist-get-marks))
              (mark-value (car-safe marks)))
         (cond
          ((null marks)
           (ilist-mark-with-char blist-default-mark))
          ((eq mark-value blist-default-mark)
           (ilist-unmark)))))
     #'ilist-get-index
     start end)))

;;;; change marks

;;;###autoload
(defun blist-change-marks (old new)
  "Change all OLD mark to NEW mark.
OLD and NEW are both characters used to mark buffers."
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (let ((inhibit-read-only t))
    (ilist-map-lines
     (lambda ()
       (ilist-mark-with-char new))
     (lambda ()
       (let* ((marks (ilist-get-marks))
              (mark-value (car-safe marks)))
         (eq mark-value old))))))

;;;; blist-unmark-forward

;;;###autoload
(defun blist-unmark-forward (&optional arg)
  "Unmark bookmarks.
If the region is active, unmark the region.

If the region is not active, then unmark ARG lines forward (or
backward).

If the point is on a header, then ARG specifies the number of
groups to unmark."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda () (ilist-mark-with-char t))
     nil start end)))

;;;; blist-unmark-backward

;;;###autoload
(defun blist-unmark-backward (&optional arg)
  "Unmark bookmarks.
If the region is active, unmark the region.

If the region is not active, then unmark ARG lines backward (or
forward).

If the point is on a header, then ARG specifies the number of
groups to unmark."
  (interactive "p")
  (blist-assert-mode)
  (blist-unmark-forward (- (prefix-numeric-value arg))))

;;;; blist-unmark-all

;;;###autoload
(defun blist-unmark-all ()
  "Unmark all bookmarks."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t))
    (ilist-map-lines #'ilist-unmark #'ilist-is-marked)))

;;;; blist-unmark-all-mark

;;;###autoload
(defun blist-unmark-all-mark (a-mark)
  "Unmark all lines marked with a specific A-MARK.
If A-MARK is RET, then unmark all marks."
  (interactive "cUnmark marks (RET means all): ")
  (blist-assert-mode)
  (cond
   ((= a-mark #xd)
    (blist-unmark-all))
   ((let ((inhibit-read-only t))
      (ilist-map-lines
       #'ilist-unmark
       (lambda ()
         (memq a-mark (ilist-get-marks))))))))

;;;; blist-delete-marked

;;;###autoload
(defun blist-delete-marked ()
  "Delete marked bookmarks from `bookmark-alist'."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t)
        (marked-list
         (sort
          (ilist-map-lines
           #'ilist-get-index
           (lambda ()
             (let* ((marks (ilist-get-marks))
                    (mark-value (car-safe marks)))
               (eq mark-value blist-default-mark))))
          #'<)))
    (cond
     ((or blist-expert
          (y-or-n-p "Confirm deletion? "))
      (cond
       (marked-list
        (setq bookmark-alist
              (ilist-delete-from-list bookmark-alist marked-list)))
       ((ilist-get-index)
        (setq
         bookmark-alist
         (ilist-delete-from-list
          bookmark-alist (list (ilist-get-index))))))
      (blist-list-bookmarks)))))

;;;; Is it marked for deletion?

(defun blist-is-marked-for-deletion-p ()
  "Return non-nil if the bookmark at point is marked for deletion."
  (declare (side-effect-free t))
  (memq blist-deletion-mark
        (mapcar
         (lambda (range)
           (get-text-property (car range) 'ilist-mark-column))
         (ilist-mark-columns (point)))))

;;;; blist-do-delete

;;;###autoload
(defun blist-do-delete ()
  "Delete bookmarks marked for deletion."
  (interactive)
  (blist-assert-mode)
  (let ((inhibit-read-only t)
        (marked-list
         (sort
          (ilist-map-lines
           #'ilist-get-index #'blist-is-marked-for-deletion-p)
          #'<)))
    (cond
     ((null marked-list)
      (user-error "No bookmarks marked for deletion"))
     ((or blist-expert
          (y-or-n-p "Confirm deletion? "))
      (setq
       bookmark-alist
       (ilist-delete-from-list bookmark-alist marked-list))
      (blist-list-bookmarks)))))

;;;; blist-mark-for-deletion

;;;###autoload
(defun blist-mark-for-deletion (arg)
  "Mark for deletion.
If the region is active, mark the region for deletion.

If the region is not active, then mark ARG lines forward (or
backward) for deletion.

If the point is on a header, then ARG specifies the number of
groups to mark for deletion."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
  (ilist-map-lines
   (lambda () (ilist-mark-with-char blist-deletion-mark))
   nil start end)))

(defun blist-mark-for-deletion-backward (arg)
  "Mark for deletion.
Like `blist-mark-for-deletion', but go backwards.

The negative of ARG is send to `blist-mark-for-deletion'."
  (interactive "p")
  (blist-mark-for-deletion
   (- (prefix-numeric-value arg))))

;;;; blist-mark

;;;###autoload
(defun blist-mark (arg)
  "Mark bookmarks.
If the region is active, mark the region.

If the region is not active, then mark ARG lines forward (or
backward).

If the point is on a header, then ARG specifies the number of
groups to mark."
  (interactive "p")
  (blist-assert-mode)
  (let* ((inhibit-read-only t)
         (temp (blist-operate-range (prefix-numeric-value arg)))
         (start (car temp))
         (end (cdr temp)))
    (ilist-map-lines
     (lambda () (ilist-mark-with-char blist-default-mark))
     nil start end)))

;;;; blist-mark-by-name

;;;###autoload
(defun blist-mark-by-name (name &optional mark-char)
  "Mark lines matching NAME with MARK-CHAR.
Interactively, query the user for NAME.  And if called with
\\[universal-argument], also query for MARK-CHAR."
  (interactive
   (list
    (read-string "Mark by name (regexp): ")
    (cond
     (current-prefix-arg
      (let ((cursor-in-echo-area t))
        (read-char "Mark with character: "))))))
  (cond
   ((not (characterp mark-char))
    (setq mark-char blist-default-mark)))
  (ilist-map-lines
   (lambda ()
     (let ((inhibit-read-only t))
       (ilist-mark-with-char mark-char)))
   (lambda ()
     (cond
      ((ilist-get-index)
       (string-match-p
        name
        (bookmark-name-from-full-record
         (nth (ilist-get-index)
              bookmark-alist))))))))

;;;; blist-mark-by-location

;;;###autoload
(defun blist-mark-by-location (name &optional mark-char)
  "Mark lines with location matching NAME with MARK-CHAR.
Interactively, query the user for NAME.  And if called with
\\[universal-argument], also query for MARK-CHAR."
  (interactive
   (list
    (read-string "Mark by name (regexp): ")
    (cond
     (current-prefix-arg
      (let ((cursor-in-echo-area t))
        (read-char "Mark with character: "))))))
  (cond
   ((not (characterp mark-char))
    (setq mark-char blist-default-mark)))
  (ilist-map-lines
   (lambda ()
     (let ((inhibit-read-only t))
       (ilist-mark-with-char mark-char)))
   (lambda ()
     (cond
      ((ilist-get-index)
       (string-match-p
        name
        (blist-get-location
         (nth (ilist-get-index) bookmark-alist))))))))

;;;; blist-next-marked

(defun blist-next-marked (arg)
  "Go to next ARG marked line."
  (interactive "p")
  (blist-assert-mode)
  (let ((orig (line-beginning-position))
        (direction (cond ((> arg 0) 1)
                         (-1)))
        (forwardp (> arg 0))
        (arg (abs arg)))
    (while (> arg 0)
      (let (res pos)
        (save-excursion
          (ilist-forward-line direction blist-movement-cycle)
          (while (and (not (ilist-boundary-buffer-p forwardp))
                      (not (= (line-beginning-position)
                              orig))
                      (not res))
            (setq res (ilist-is-marked))
            (cond (res (setq pos (point))))
            (ilist-forward-line direction blist-movement-cycle)))
        (cond
         (pos (goto-char pos))
         ((user-error "No marked bookmark"))))
      (setq arg (1- arg)))))

;;;; blist-prev-marked

(defun blist-prev-marked (arg)
  "Go to previous ARG marked line."
  (interactive "p")
  (blist-next-marked
   (- (prefix-numeric-value arg))))

;;;; blist-prev-group

;;;###autoload
(defun blist-prev-group (arg)
  "Go to previous ARG group."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-group-header arg blist-movement-cycle))

;;;; blist-next-group

;;;###autoload
(defun blist-next-group (arg)
  "Go to next ARG group."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-group-header arg blist-movement-cycle))

;;;; blist-next-item

;;;###autoload
(defun blist-next-item (arg)
  "Go to next ARG item.
An item means a line that is not a group header.

It might stop at a non-item line, if there is no such line to
stop at."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-line arg blist-movement-cycle t))

;;;; blist-prev-item

;;;###autoload
(defun blist-prev-item (arg)
  "Go to previous ARG item.
An item means a line that is not a group header.

It might stop at a non-item line, if there is no such line to
stop at."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-line arg blist-movement-cycle t))


;;;; blist-prev-line

;;;###autoload
(defun blist-prev-line (arg)
  "Go to previous ARG line."
  (interactive "p")
  (blist-assert-mode)
  (ilist-backward-line arg blist-movement-cycle nil))

;;;; blist-next-line

;;;###autoload
(defun blist-next-line (arg)
  "Go to next ARG line."
  (interactive "p")
  (blist-assert-mode)
  (ilist-forward-line arg blist-movement-cycle nil))

(provide 'blist)
;;; blist.el ends here
