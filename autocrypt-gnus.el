;;; autocrypt-gnus.el --- Autocrypt for Gnus -*- lexical-binding:t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:

;; MUA specific functions for Gnus
;;
;; Setup with (add-hook 'gnus-load-hook #'autocrypt-mode)

;;; Code:

(require 'gnus)
(require 'autocrypt)

;;;###autoload
(defun autocrypt-gnus--install ()
  "Prepare autocrypt for Gnus."
  (add-hook 'gnus-article-prepare-hook #'autocrypt-process-header nil t))

(defun autocrypt-gnus--uninstall ()
  "Undo `autocrypt-gnus--install'."
  (remove-hook 'gnus-article-prepare-hook #'autocrypt-process-header t))

(defun autocrypt-gnus--get-header (header)
  "Return value for HEADER from current message."
  (gnus-fetch-original-field header))

(provide 'autocrypt-gnus)

;;; autocrypt-gnus.el ends here
