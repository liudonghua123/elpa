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
  :type '(list (cons string function)))

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

;;; Variables

;;;; Sorter

(defvar blist-sorter blist-default-sorter
  "The function used to sort the bookmark list.
See `ilist-string' for how the sorter should behave.")

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
      (let ((inhibit-read-only t))
        (widen)
        (delete-region (point-min) (point-max))
        (insert
         (ilist-string
          bookmark-alist
          (append
           (list ilist-mark-column
                 (blist-name-column))
           (cond
            (blist-display-location-p (list blist-location-column))))
          blist-filter-groups
          blist-discard-empty-p
          blist-sorter))
        (insert (string #xa)))
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
  (define-key map (vector #x20) #'forward-line)
  (define-key map (vector ?p) #'blist-prev-line)
  (define-key map (vector ?N) #'blist-next-item)
  (define-key map (vector ?P) #'blist-prev-item)
  (define-key map (vector ?\M-n) #'blist-next-group)
  (define-key map (vector ?\M-p) #'blist-prev-group)
  (define-key map (vector 'tab) #'blist-next-group)
  (define-key map (vector 'backtab) #'blist-prev-group)
  (define-key map (vector 'return) #'blist-return)
  (define-key map (vector ?o) #'blist-open-other-window)
  (define-key map (vector #x29) #'blist-next-marked)
  (define-key map (vector #x28) #'blist-prev-marked)
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
                  (ilist-forward-line arg)
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
     (nth (ilist-get-index) bookmark-alist)))))

;;;; Open in another window

;;;###autoload
(defun blist-open-other-window ()
  "Open the bookmark at point in another window."
  (interactive)
  (blist-assert-mode)
  (cond ((ilist-get-index)
         (bookmark--jump-via
          (nth (ilist-get-index) bookmark-alist)
          #'switch-to-buffer-other-window))))

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
   ((ilist-get-group)
    (blist-toggle-group))
   ((ilist-get-index) (blist-open))
   ((user-error "Nothing to do here"))))

;;;; Toggle location display

;;;###autoload
(defun blist-toggle-location ()
  "Toggle the display of locations of bookmarks."
  (interactive)
  (let (temp)
    (setq-local blist-display-location-p
                (not blist-display-location-p))
    (setq temp blist-display-location-p)
    (blist-list-bookmarks)
    ;; restore the value of the variable
    (setq-local blist-display-location-p temp)))

;;;; All bookmarks, including hidden ones

(defun blist-all-bookmarks ()
  "Return the list of all bookmark indices, even the hidden ones."
  (append
   ;; normal lines
   (ilist-map-lines
    (lambda ()
      (bookmark-name-from-full-record
       (nth (ilist-get-index) bookmark-alist)))
    #'ilist-get-index)
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
          (ilist-map-lines
           (lambda ()
             (bookmark-name-from-full-record
              (nth (ilist-get-index) bookmark-alist)))
           #'ilist-get-index
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
     (blist-all-bookmarks) nil t)))
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
