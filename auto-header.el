;;; auto-header.el --- Automatically find the right headers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Philip Kaludercic

;; Author: Philip Kaludercic <philip.kaludercic@fau.de>
;; Version: $Id: auto-header.el,v 1.18 2022/06/13 19:48:18 oj14ozun Exp oj14ozun $
;; URL: https://wwwcip.cs.fau.de/~oj14ozun/src+etc/auto-header.el
;; Package-Version: 1
;; Keywords: c

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

;; This script parses man-pages to guess what header files a function
;; might need.

;; Bind `auto-header-at-point' to a convenient key if you want to
;; invoke the functionality manually, or add `auto-header-buffer' to
;; `before-save-hook' (in most cases you want to only add it locally),
;; so that headers are updated just before saving the contents of a
;; buffer.  This can also be done by enabling `auto-header-mode', or
;; by adding it to a major mode hook (e.g. `c-mode-hook').

;;; Code:

(eval-when-compile (require 'rx))

(defvar auto-header--cache (make-hash-table :test 'equal)
  "Hash table associating function to a list of header files.")

(defun auto-header--parse-manpage (name)
  "Parse and cache the results for the manpage NAME."
  (with-temp-buffer
    (when (zerop (call-process "man" nil t nil "-s" "2,3,3p" name))
      (goto-char (point-min))
      (search-forward-regexp "^SYNOPSIS")
      (forward-line)
      (let ((start (point)))
        (search-forward-regexp "^DESCRIPTION")
        (narrow-to-region start (point)))
      (goto-char (point-min))
      (let (next headers)
        (while (not (eobp))
          (cond
           ((and (looking-at-p (rx bol (* blank) eol)) next)
            (setq headers nil
                  next nil))
           ((looking-at (rx bol (* blank)
                            "#include <"
                            (group (+ nonl))
                            ">" eol))
            (push (match-string 1) headers))
           ((looking-at (rx bol (* blank)
                            (+? nonl) (+ blank)
                            (group (+ alnum))
                            "("))
            (unless (gethash (match-string 1) auto-header--cache)
              (puthash (match-string 1)
                       (delete-dups headers)
                       auto-header--cache)
              (setq next t))))
          (forward-line)))))
  (gethash name auto-header--cache))

(defun auto-header-find-headers (name)
  "Return a list of headers for that have to be used for NAME."
  (or (gethash name auto-header--cache)
      (auto-header--parse-manpage name)))

(defun auto-header--insert-headers (headers)
  "Insert HEADERS at the beginning of the current buffer."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (when (forward-comment 1)
	(search-forward-regexp "^.+$" nil t)
	(forward-paragraph))
      (dolist (header headers)
	(insert "#include <" header ">")
	(newline))
      (goto-char (point-max))
      (when (search-backward-regexp "#include ?<" nil t)
	(save-restriction
	  (let ((end (line-end-position)))
	    (goto-char (point-min))
	    (forward-comment 1)
	    (narrow-to-region (point) end)
	    (delete-duplicate-lines (point-min) (point-max) nil nil t)
	    (sort-lines nil (point-min) (point-max)))
	  (goto-char (point-max))
	  (newline))
	(delete-blank-lines)))))

(defvar auto-header--header-cache nil
  "Cache of known header files.")

(defun auto-header--header-list ()
  "Return a list of known header files."
  (catch 'fail
    (unless auto-header--header-cache
      (with-temp-buffer
	(process-file (or (executable-find "gcc")
			  (executable-find "clang")
			  (throw 'fail nil))
		      nil t nil
		      "-xc" "-E" "-v" "-")
	(goto-char (point-min))
	(search-forward "#include <...> search starts here:")
	(forward-line 1)
	(let ((start (point)))
	  (search-forward "End of search list.")
	  (beginning-of-line)
	  (narrow-to-region start (point)))
	(goto-char (point-min))
	(while (search-forward-regexp (rx bol (* space) (group (+ nonl)) eol)
				      nil t)
	  (let ((dir (match-string 1)))
	    (setq auto-header--header-cache
		  (nconc
		   (mapcar
		    (lambda (file) (file-relative-name file dir))
		    (directory-files-recursively dir "\\.h$"))
		   auto-header--header-cache)))))
      (setq auto-header--header-cache
	    (delete-dups auto-header--header-cache)))
    auto-header--header-cache))

(defun auto-header-insert (header)
  "Add HEADER to the top of the file."
  (interactive (completing-read "Add header: " (auto-header--header-list)))
  (auto-header--insert-headers (list header)))

;;;###autoload
(defun auto-header-at-point (name)
  "Insert headers for NAME.
When called interactively, use the symbol at or before point as
NAME."
  (interactive
   (list (or (thing-at-point 'symbol t)
             (if (save-excursion
                   (search-backward-regexp "\\_<\\(.+?\\)\\_>" nil t))
                 (match-string-no-properties 1)
               (user-error "No symbol")))))
  (let ((headers (auto-header-find-headers name)))
    (auto-header--insert-headers headers)
    (message "Found %d headers for %s" (length headers) name)))

(defvar auto-header--c-keywords
  ;; https://en.cppreference.com/w/c/keyword
  '("auto" "break" "case" "char" "const" "continue" "default"
    "do" "double" "else" "enum" "extern" "float" "for" "goto"
    "if" "inline" "int" "long" "register" "restrict" "return"
    "short" "signed" "sizeof" "static" "struct" "switch"
    "typedef" "union" "unsigned" "void" "volatile" "while"
    "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex"
    "_Decimal128" "_Decimal32" "_Decimal64" "_Generic"
    "_Imaginary" "_Noreturn" "_Static_assert" "_Thread_local"
    "bool" "true" "false")
  "List of keywords to not treat as functions.")

;;;###autoload
(defun auto-header-buffer ()
  "Insert the headers for all functions."
  (interactive)
  (save-excursion
    (let (headers)
      (goto-char (point-min))
      (while (search-forward-regexp
              (rx symbol-start
                  (group alpha (* (or alnum ?_)))
                  symbol-end
                  (* space) "(")
              nil t)
        (unless (or (member (match-string-no-properties 1)
                            auto-header--c-keywords)
                    (nth 4 (syntax-ppss)))
          (push (match-string-no-properties 1) headers)))
      (auto-header--insert-headers
       (apply #'append (mapcar #'auto-header-find-headers
                               headers))))))

;;;###autoload
(define-minor-mode auto-header-mode
  "Run `auto-header-buffer' before saving."
  :lighter " AH"
  (if auto-header-mode
      (progn
        (unless (derived-mode-p 'c-mode)
          (warn "Auto-header ought to be used in a C-mode buffer"))
        (add-hook 'before-save-hook #'auto-header-buffer nil t))
    (remove-hook 'before-save-hook #'auto-header-buffer t)))

(provide 'auto-header)
;;; auto-header.el ends here
