;;; gnat-alire.el --- Support for building with Alire -*- lexical-binding:t -*-
;;
;;; Copyright (C) 2012 - 2022  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; See https://alire.ada.dev/

(require 'gnat-compiler)

(defun alire-get-env (project)
  "Set PROJECT slots from Alire as needed."

  ;; alire inherits GPR_PROJECT_PATH (see
  ;; https://github.com/alire-project/alire/issues/1147). So empty it
  ;; here.
  ;;
  ;; We need all of the alire settings for "gnat list" and "gpr_query"
  ;; to properly process complex projects (like Alire).
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "GPR_PROJECT_PATH" "")

    (with-temp-buffer
      (let ((status (call-process "alr" nil (current-buffer) nil "printenv")))
	(cond
	 ((= 0 status)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (looking-at "export \\(.*\\)$")
	    (setf (wisi-prj-file-env project)
		  (append (wisi-prj-file-env project) (list (match-string-no-properties 1))))
	    (forward-line 1)
	    ))

	 (t
	  (user-error "alr printenv failed; bad or missing alire.toml?"))
	 ))
      )))

;;;###autoload
(cl-defun create-alire-project (&key name gpr-file compile-env xref-label)
  ;; We could use "alr exec -P -- echo" to get the project file (also
  ;; see https://github.com/alire-project/alire/issues/1151), but that
  ;; doesn't work when there are multiple project files listed in
  ;; alire.toml. And if there are multiple project files, the user
  ;; needs to pick one anyway.  So we require it as an argument; must
  ;; be absolute or relative to Alire root directory.
  ;;
  ;; prj-file should _not_ specify the gpr-file or gpr-project-path;
  ;; it is only used for casing. We get GPR_PROJECT_PATH from the
  ;; Alire environment.
  "Return an initial wisi project for the current Alire workspace."
  (let* ((default-directory (locate-dominating-file default-directory "alire.toml"))
	 (abs-gpr-file (expand-file-name gpr-file))
	 (project (make-wisi-prj :name name :compile-env compile-env))
	 )

    (alire-get-env project)

    ;; We need a gnat-compiler to set compilation-search-path.
    (setf (wisi-prj-compiler project)
	  (create-gnat-compiler
	   :gpr-file abs-gpr-file
	   :run-buffer-name (gnat-run-buffer-name abs-gpr-file)))

    (setf (wisi-prj-xref project)
	  (funcall (intern (format "create-%s-xref" (symbol-name xref-label)))
		   :gpr-file abs-gpr-file))

    project))

(provide 'gnat-alire)
;;; gnat-alire.el ends here
