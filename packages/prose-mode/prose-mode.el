;;; prose-mode.el --- Minor mode for writing prose   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Version: 0

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

;; This package provides the minor mode `prose-mode', which can be
;; enabled in buffers based on text-mode to provide more intuitive
;; natural language editing (for languages that use words and
;; sentences).

;; It provides key-bindings for some useful existing commands like
;; `transpose-sentences', `forward-to-word', etc.  It also tries to be
;; smart about adjusting inter-word spacing after killing and yanking
;; text.  Lastly there are a few special commands:

;; `prose-mode-transpose-words': Transpose the words to left and right
;; of the word under point.

;; `prose-mode-kill-word': Kill the whole word(s) under point,
;; regardless of where point is in the word.

;; `prose-mode-merge-sentences': Merge the two sentences around point.

;; `prose-mode-split-sentences': Split the current sentence at point.

;;; Code:

;; Note: we could set up our own `find-word-boundary-function-table'
;; so that forward- and backward-word don't skip over multiple
;; newlines and all that.

(defvar prose-mode-map
  (eval-when-compile
    (let ((km (make-sparse-keymap "Prose mode")))
      (define-key km (kbd "M-T") #'prose-mode-transpose-words)
      (define-key km (kbd "M-D") #'prose-mode-kill-word)
      ;; This is "C-M-T" so as not to conflict with org-mode's
      ;; "C-M-t", which runs `org-transpose-element'.
      (define-key km (kbd "C-M-T") #'transpose-sentences)
      (define-key km (kbd "C-M-f") #'forward-sentence)
      (define-key km (kbd "C-M-b") #'backward-sentence)
      (define-key km (kbd "M-F") #'forward-to-word)
      (define-key km (kbd "M-B") #'backward-to-word)
      (define-key km (kbd "M-m") #'prose-mode-merge-sentences)
      (define-key km (kbd "M-M") #'prose-mode-split-sentence)
km)))

(define-minor-mode prose-mode
  "Minor mode providing prose-oriented editing features."
  nil " prose"
  prose-mode-map
  (advice-add
   'canonically-space-region
   :filter-args
   #'prose-mode-space-canonically)
  (advice-add
   'kill-region
   :after
   #'prose-mode-space-area)
  (advice-add
   'kill-region
   :after
   #'prose-mode-add-yank-handler))

(defun prose-mode-add-yank-handler (&rest _args)
  ;; There doesn't seem to be a good way to get the actual killed
  ;; string from `kill-region', so hack it with `kill-ring'.
  (when prose-mode
    (add-text-properties
     0 (length (car kill-ring))
     '(yank-handler (prose-mode-yank-handler)) (car kill-ring))))

(defun prose-mode-yank-handler (str)
  "Fix spaces around yanked string STR."
  (if prose-mode
      (let ((start (point)))
	;; Probably need to be smarter about bob/bol/eol/eob.
	(insert (concat " " str " "))
	(prose-mode-space-area start (point)))
    (insert str)))

(defun prose-mode-space-area (&optional start end)
  "Canonically space an area encompassing START and END."
  (when prose-mode ; Because it's also an advice.
    (save-excursion
      ;; We've advised this.
      (canonically-space-region (progn
				  (when start
				    (goto-char start))
				  (backward-word 1) (point))
				(progn
				  (when end
				    (goto-char end))
				  (forward-word 2) (point))))))

(defun prose-mode-space-canonically (bounds)
  "Canonically space the region, delimited by BOUNDS.
This function does some extra space manipulation, before handing
off to `canonically-space-region'.  At present, it removes extra
spaces before punctuation such as commas and full stops, and
around a run of three dashes (probably representing an em-dash)."
  ;; Because it will likely alter the size of the region, this is
  ;; added as a :filter-args function, and must return the new values
  ;; of BOUNDS as a list.
  (let ((punct-re "[[:space:]]+\\([,.!?;]\\)")
	(dash-re "[[:space:]]?\\(---\\)[[:space:]]?")
	(start-marker (set-marker (make-marker) (car bounds)))
	(end-marker (set-marker (make-marker) (nth 1 bounds))))
    (when prose-mode
      (save-excursion
	(goto-char start-marker)
	(while (re-search-forward punct-re end-marker t)
	  (replace-match "\\1"))
	(goto-char start-marker)
	(while (re-search-forward dash-re end-marker t)
	  (replace-match "\\1"))))
    (list (marker-position start-marker)
	  (marker-position end-marker))))

(defun prose-mode-transpose-words ()
  "Transpose words around the word under point.
Works as if `transpose-words' had been called with a 0 prefix
arg."
  (interactive)
  (let ((orig (or (command-remapping #'transpose-words)
		  #'transpose-words))
	handle-caps)
    ;; This rigmarole is necessary because the 0 prefix arg actually
    ;; means "exchange the words at point and mark", not "exchange the
    ;; words around the word under point".  This function could
    ;; conceivably accept a numeric prefix arg, and go that number of
    ;; words out in both directions before transposing, but would
    ;; anyone really want that?
    (save-excursion
      (backward-word (if (looking-at-p "\\<") 1 2))
      (push-mark)
      (setq handle-caps
	    ;; This word is capitalized because it's at the beginning
	    ;; of a sentence.  Or it's a proper name.  Or...
	    (and (looking-at-p "[A-Z]")
		 (= (point)
		    (save-excursion
		      (forward-sentence)
		      (backward-sentence)))))
      (when handle-caps
	(save-excursion (downcase-word 1))))
    (save-excursion
      (when handle-caps
	(save-excursion
	  (forward-to-word 1)
	  (capitalize-word 1)))
      (forward-word (if (looking-at-p "\\>") 1 2))
      ;; If point is at the end of a word and punctuation follows, the
      ;; punctuation gets dragged along with the word unless we back
      ;; up one character.
      (backward-char)
      (funcall orig 0))
    (pop-mark)))

(defun prose-mode-kill-word (arg)
  "Kill word(s) under point.
Behaves exactly like `kill-word', but kills an entire word
regardless of where point is on the word."
  (interactive "p")
  (if (> arg 0)
      (skip-syntax-backward "w")
    (skip-syntax-forward "w"))
  (kill-word arg))

(defun prose-mode-split-sentence ()
  "Split sentence at point."
  (interactive)
  (unless (looking-at-p "\\>")
    (forward-word))
  (insert ".")
  (unless (save-excursion
	    (backward-char)
	    (looking-at-p (sentence-end)))
    (insert " "))
  (prose-mode-space-area)
  (capitalize-word 1)
  (backward-word))

(defun prose-mode-merge-sentences ()
  "Merge the sentences around point."
  (interactive)
  (unless (or (= (point)
		 (save-excursion
		   (backward-sentence)
		   (forward-sentence)
		   (point)))
	      (= (point)
		 (save-excursion
		   (forward-sentence)
		   (backward-sentence)
		   (point)))
	      (looking-at-p (sentence-end)))
    (user-error "No sentences to merge"))
  (unless (looking-at-p "\\.")
    (re-search-backward "\\." nil t))
  (delete-char 1)
  (save-excursion
    (downcase-word 1))
  (prose-mode-space-area))

(provide 'prose-mode)
;;; prose-mode.el ends here
