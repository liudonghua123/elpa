;;; do-at-point.el --- Generic context-sensitive action dispatcher.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Philip Kaludercic

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; URL: https://wwwcip.cs.fau.de/~oj14ozun/src+etc/do-at-point.el
;; Version: $Id: do-at-point.el,v 1.3 2023/07/16 11:34:38 oj14ozun Exp oj14ozun $
;; Package-Version: 1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience

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

;; The command `do-at-point' is a generalised `find-file-at-point',
;; both in the sense that it can understand more than just files, and
;; do more than just open a file.  Depending on the "thing" at point,
;; different "actions" can be dispatched, e.g. opening a url using
;; `browse-url' or occurring a symbol at point.

;; The entry point of this package is `do-at-point'.  Bind it to a
;; convenient key:
;;
;;   (global-set-key (kbd "C-'") #'do-at-point)
;;
;; Most of the behaviour is controlled via the user option
;; `do-at-point-actions'.

;;; Relation to "Embark"

;; A similar package, that served as inspiration for `do-at-point' is
;; called Embark (https://github.com/oantolin/embark), by Omar Antolín
;; Camarena.  I'll be honest, I don't get the hype but I find the core
;; functionality nice.  This package is a fraction of the size of
;; Embark, but does pretty much exactly what I would want to use
;; Embark for + it dispatches actions using `read-multiple-choice'
;; instead of popping up a buffer of using `completing-read' -- which
;; is a bad choice for users who aren't using narrowing-selecting
;; completion frameworks.

;;; Code:

(require 'seq)

(defgroup do-at-point '()
  "Generic context-sensitive action dispatcher."
  :group 'convenience)

(defcustom do-at-point-actions
  `((region
     (?\C-i "Indent" ,#'indent-region)
     (?s "Isearch"
	 ,(lambda (str)
	    (isearch-mode t)
	    (isearch-yank-string str)))
     (?o "Occur" ,(lambda (str) (occur (regexp-quote str))))
     (?w "Kill-Save" ,#'kill-new)
     (?k "Kill" ,#'kill-region)
     (?n "Narrow" ,#'narrow-to-region)
     (?$ "Spell check" ,#'ispell-region)
     (?| "Pipe command"
	 ,(lambda (beg end)
	    (let ((cmd (read-shell-command "Command: ")))
	      (shell-command-on-region beg end cmd))))
     (?! "Shell command" ,#'shell-command))
    (email
     (?m "Compose message" ,(lambda (to) (compose-mail to))))
    (existing-filename
     (?f "Find file" ,#'find-file)
     (?4 "Find file other window" ,#'find-file-other-window))
    (url
     (?f "Open" ,#'browse-url)
     (?d "Download" ,#'(lambda (url)
			 (start-process "*Download*" nil "wget" url))))
    (symbol
     (?. "Xref" ,#'xref-find-definitions)
     (?o "Occur" ,(lambda (str)
		    (occur (concat "\\_<\\(" (regexp-quote str) "\\)\\_>")))))
    (word
     (?$ "Spell check" ,(lambda () (ispell-word)))
     (?d "Dictionary" ,#'dictionary-search))
    (string) (sexp)
    (defun
	(?e "Evaluate" ,(lambda () (eval-defun nil)))))
  "Association of things and their respective actions.
Each element of the list has the form (THING . ACTIONS), where
THING is a symbol as interpreted by `thing-at-point' and ACTIONS
have the form (KEY NAME FUNC), where KEY is a dispatch character,
NAME is a brief description of the action and FUNC is a function
that will be dispatched when KEY is selected.  FUNC can take
zero, one or two arguments, which `do-at-point' will respectively
interpret as function that is invoked without any arguments, or
with a buffer substring or the bounds of THING.  Actions listed
under the \"thing\" `region' are shared among all \"things\".
This is why a an entry does not require any actions to be
associated with it, if it just serves as a specific kind of
region worth selecting.  The order of element in the list
correspond to the order in which `do-at-point' will prompt the
user for possible things at point."
  :type '(alist :value-type
		(alist :value-type
		       (list :tag "Action"
			     (string :tag "Description") function)
		       :key-type character)
		:key-type symbol))

(defcustom do-at-point-quick-select '(?\C-m)
  "List of keys to quickly select the first action."
  :type '(repeat character))

(defconst do-at-point--overlay
  (let ((ov (make-overlay 0 0)))
    (overlay-put ov 'face 'highlight)
    (delete-overlay ov)
    ov))

;;;###autoload
(defun do-at-point ()
  "Dispatch an action on the thing at point."
  (interactive)
  (unwind-protect
      (let* ((things (mapcar #'car do-at-point-actions))
	     (cand (seq-filter #'thing-at-point things))
	     (last last-input-event) (key last) (i 0) thing)
	(when (null cand)
	  (user-error "Nothing actionable at point"))
	(while (eq key last)
	  (setq thing (nth (mod i (length cand)) cand))
	  (let ((bound (bounds-of-thing-at-point thing))
		(default (cadar (or (alist-get thing do-at-point-actions)
				    (alist-get 'region do-at-point-actions)))))
	    (move-overlay do-at-point--overlay (car bound) (cdr bound))
	    (setq key (read-key (if (and do-at-point-quick-select default)
				    (format "Act on `%s' (%s by default)?" thing default)
				  (format "Act on `%s'?" thing)))
		  i (1+ i))
	    (when (eq key ?\C-g) (keyboard-quit))))
	(let* ((options (append
			 (and (not (eq thing 'region))
			      (alist-get thing do-at-point-actions))
			 (alist-get 'region do-at-point-actions)))
	       (choice
		(if (memq key do-at-point-quick-select)
		    (car options)
		  (when (assq key options)
		    (push key unread-post-input-method-events))
		  (read-multiple-choice
		   (format "Action on %s" thing)
		   (seq-uniq
		    (mapcar (lambda (ent)
			      (list (car ent) (cadr ent) (cadddr ent)))
			    options)
		    (lambda (a b) (eq (car a) (car b)))))))
	       (func (cadr (alist-get (car choice) options)))
	       (bound (bounds-of-thing-at-point thing)))
	  (message nil)		;clear minibuffer
	  (pcase (car (func-arity func))
	    (0 (funcall func))
	    (1 (funcall func (buffer-substring (car bound) (cdr bound))))
	    (2 (funcall func (car bound) (cdr bound)))
	    (_ (error "Unsupported signature: %S" func)))))
    (delete-overlay do-at-point--overlay)))

(provide 'do-at-point)
;;; do-at-point.el ends here
