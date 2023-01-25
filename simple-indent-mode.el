;;; simple-indent-mode.el --- Major mode for mmm-mode regions that need indent -* lexical-binding:t -*-


;; Copyright (C) 2017 - 2022  Free Software Foundation, Inc.

;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages
;; Version: 1.2.0
;; package-requires: ((wisi "3.1.1") (emacs "25.0") (mmm-mode "0.5.7"))
;; url: https://www.nongnu.org/ada-mode/

;; This file is part of GNU Emacs.

;; wisitoken-grammar-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; wisitoken-grammar-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:

(defgroup simple-indent nil
  "Major mode for editing Wisi grammar files in Emacs."
  :group 'languages)

(defcustom simple-indent 3
  "Indent for all lines in `simple-indent-mode'."
  :type 'integer
  :group 'simple-indent)

(defvar mmm-save-local-variables)
(add-to-list 'mmm-save-local-variables (list 'simple-indent 'buffer (list 'simple-indent-mode)))

(defun simple-indent-region (begin end)
  "For `indent-region-function'."
  (goto-char begin)
  (while (and (not (eobp))
	      (or (and (= begin end) (= (point) end))
		  (>= (point) begin)))
    (back-to-indentation)
    (unless (bolp)
      ;; If already in column 0, keep it there. This allows this
      ;; indent style in wisitoken-grammar-mode:
      ;; %{
      ;;   code
      ;;   code
      ;; }%
      (indent-line-to simple-indent))
    (forward-line 1)))

(defun simple-indent-line ()
  "For `indent-region-function'."
  (indent-line-to simple-indent))

;;;;
;;;###autoload
(define-derived-mode simple-indent-mode prog-mode "simple"
  "A major mode for mmm-mode regions that need indent."
  (set (make-local-variable 'indent-line-function) #'simple-indent-line)
  (set (make-local-variable 'indent-region-function) #'simple-indent-region))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wy\\'" . simple-indent-mode))

;; Tie the mode to the defcustoms above.
(put 'simple-indent-mode 'custom-mode-group 'simple-indent)

(provide 'simple-indent-mode)
;;; simple-indent-mode.el ends here
