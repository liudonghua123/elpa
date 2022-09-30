;;; rcirc-color.el --- color nicks  -*- lexical-binding:t -*-

;; Copyright (C) 2005-2022  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 0.4.3
;; Package-Requires: ((emacs "24.4"))
;; Keywords: comm

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Use /COLOR to list all colored nicks with their color
;; Use /COLOR NICK to color NICK randomly
;; Use /COLOR NICK COLOR to color NICK using COLOR

;;; Code:

(require 'rcirc)

(defgroup rcirc-color ()
  "Highlight nicknames in rcirc."
  :group 'rcirc)

(defun rcirc-color-distance (color1 color2)
  "Compute the difference between two colors.
The difference between COLOR1 and COLOR2 is computed using the
weighted Euclidean distance formula proposed on
<http://www.compuphase.com/cmetric.htm>.  Remember that every
component for the formula is in the range of 0-xFF and
`color-values' will return a range of 0-FFFF.  Thus, divide
everything by 256. This also helps preventing integer overflow."
  (let* ((dr (/ (- (nth 0 (color-values color1))
		   (nth 0 (color-values color2)))
		256))
	 (dg (/ (- (nth 1 (color-values color1))
		   (nth 1 (color-values color2)))
		256))
	 (db (/ (- (nth 2 (color-values color1))
		   (nth 2 (color-values color2)))
		256))
	 (red-mean (/ (+ (nth 0 (color-values color1))
			 (nth 0 (color-values color2)))
		      2 256)))
    (sqrt (+ (ash (* (+ 512 red-mean) dr dr) -8)
	     (* 4 dg dg)
	     (ash (* (- 767 red-mean) db db) -8)))))

(defcustom rcirc-colors
  (let ((min-distance 200); heuristics
	(bg (face-background 'default))
	(fg (face-foreground 'rcirc-my-nick))
	candidates)
    ;; Some themes don't specify a background for terminal displays (so that
    ;; the terminal's transparency is still effective).  In that case, assume
    ;; white or black depending on the foreground.
    (when (string= bg "unspecified-bg")
      (setq bg (if (color-dark-p (color-name-to-rgb fg)) "white" "black")))
    (dolist (item color-name-rgb-alist)
      (let ((color (car item)))
	(when (and (not (color-gray-p color))
		   (> (rcirc-color-distance color bg) min-distance)
		   (> (rcirc-color-distance color fg) min-distance))
	  (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  This uses `rcirc-color-distance'
to compute distance between colors.

To check out the list, evaluate (list-colors-display rcirc-colors)."
  :type '(repeat color))

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defcustom rcirc-color-is-deterministic nil
  "Normally rcirc just assigns random colors to nicks.
These colors are based on the list in `rcirc-colors'.
If you set this variable to a non-nil value, an md5 hash is
computed based on the nickname and the first twelve bytes are
used to determine the color: #rrrrggggbbbb."
  :type 'boolean)

(defcustom rcirc-color-other-attributes nil
  "Other attributes to use for nicks.
Example: (setq rcirc-color-other-attributes \\='(:weight bold))"
  :type 'plist)


(defun rcirc-color--facify (orig-fun string face &rest args)
  "Add colors to other nicks based on `rcirc-colors'."
  (when (and (eq face 'rcirc-other-nick)
             (> (length string) 0))
    (let ((cell (or (gethash string rcirc-color-mapping)
                    (puthash (substring-no-properties string)
                             `(:foreground
			       ,(if rcirc-color-is-deterministic
				    (concat "#" (substring (md5 string) 0 12))
				  (elt rcirc-colors
                                       (random (length rcirc-colors))))
			       ,@rcirc-color-other-attributes)
                             rcirc-color-mapping))))
      (setq face (list cell))))
  (apply orig-fun string face args))

(defun rcirc-markup-nick-colors (_sender _response)
  "Add a face to all known nicks in `rcirc-color-mapping'.
This ignores SENDER and RESPONSE."
  (with-syntax-table rcirc-nick-syntax-table
    (while (re-search-forward "\\w+" nil t)
      (let ((face (gethash (match-string-no-properties 0) rcirc-color-mapping)))
	(when face
	  (rcirc-add-face (match-beginning 0) (match-end 0) face))))))

(defun-rcirc-command color (args)
  "Change one of the nick colors."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-color (car args) (cadr args) process target))

(defun rcirc-do-color (nick color process target)
  "Implement the /color command.
NICK is the nick for which the new color ist set; if nil, all the
nicks in `rcirc-color-mapping' are shown with their corresponding
faces.

COLOR is the color to use as the new foreground-color.  If COLOR
is not supplied, a random color from `rcirc-colors' is used
instead.

PROCESS and TARGET are the standard arguments for rcirc
commands."
  (if (not nick)
      (let (names)
        (maphash (lambda (key value)
                   (add-text-properties
                    0 (length key)
                    `(face (,value) help-echo ,(cdr value))
                    key)
                   (setq names (cons key names)))
                 rcirc-color-mapping)
        (rcirc-print process (rcirc-nick process) "NOTICE" target
                     (mapconcat 'identity names " ")))
    (let* ((index (random (length rcirc-colors)))
           (color (elt rcirc-colors index))
           (face `(:foreground ,color ,@rcirc-color-other-attributes)))
      (puthash nick face rcirc-color-mapping))))

(defun rcirc-color--handler-NICK (_process sender args _text)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
         (cell (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (when cell
      (puthash new-nick cell rcirc-color-mapping))))

;;;###autoload
(define-minor-mode rcirc-color-mode
  "Enable the highlighting of nicknames."
  (cond
   (rcirc-color-mode
    (advice-add 'rcirc-facify :around #'rcirc-color--facify)
    (advice-add 'rcirc-handler-NICK :before #'rcirc-color--handler-NICK)
    (add-hook 'rcirc-markup-text-functions #'rcirc-markup-nick-colors))
   (t                                   ;disable `rcirc-color-mode'
    (advice-remove 'rcirc-facify #'rcirc-color--facify)
    (advice-remove 'rcirc-handler-NICK #'rcirc-color--handler-NICK)
    (remove-hook 'rcirc-markup-text-functions #'rcirc-markup-nick-colors))))

;; FIXME: Traditionally rcirc-color initialises itself when loaded, so
;; we preserve this behaviour even after the addition of
;; `rcirc-color-mode'.  Eventually we should move from this kind of
;; implicit to an explicit initialisation via the minor mode.  But for
;; now we just enable the minor mode to avoid breaking stuff.
(rcirc-color-mode t)

(provide 'rcirc-color)

;;; rcirc-color.el ends here
