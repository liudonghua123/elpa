;;; autocrypt-rmail.el --- Autocrypt for Rmail -*- lexical-binding:t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:

;; MUA specific functions for Rmail
;;
;; Setup with (add-hook 'rmail-mode-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt)
(require 'rmail)

(defun autocrypt-rmail--install ()
  "Install autocrypt functions into the current rmail buffer."
  (add-hook 'rmail-show-message-hook #'autocrypt-process-header nil t))

(defun autocrypt-rmail--uninstall ()
  "Remove autocrypt from current buffer."
  (add-hook 'rmail-show-message-hook #'autocrypt-process-header t))

(defun autocrypt-rmail--get-header (header)
  "Ask Rmail to return HEADER."
  (rmail-apply-in-message
   rmail-current-message
   (lambda () (mail-fetch-field header))))

(provide 'autocrypt-rmail)

;;; autocrypt-rmail.el ends here
