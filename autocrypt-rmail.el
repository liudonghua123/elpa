;;; autocrypt-rmail.el --- Autocrypt for Rmail -*- lexical-binding:t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.4.0
;; Keywords: comm
;; Package-Requires: ((emacs "25.1"))
;; URL: https://git.sr.ht/~zge/autocrypt

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

(require 'rmail)

;;;###autoload
(defun autocrypt-rmail-install ()
  "Install autocrypt hooks for Rmail."
  (add-hook 'rmail-show-message-hook #'autocrypt-process-header))

(defun autocrypt-rmail-uninstall ()
  "Remove autocrypt hooks for Rmail."
  (remove-hook 'rmail-show-message-hook #'autocrypt-process-header))

(defun autocrypt-rmail-header (field)
  "Ask Rmail to return header FIELD."
  (rmail-apply-in-message
   rmail-current-message
   (lambda () (mail-fetch-field field))))

(provide 'autocrypt-rmail)

;;; autocrypt-rmail.el ends here
