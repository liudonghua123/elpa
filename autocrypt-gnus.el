;;; autocrypt-gnus.el --- Autocrypt for Gnus -*- lexical-binding:t -*-

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

;; MUA specific functions for Gnus
;;
;; Setup with (add-hook 'gnus-load-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt)
(require 'gnus)

;;;###autoload
(defun autocrypt-gnus-install ()
  "Install autocrypt hooks for Gnus."
  (add-hook 'gnus-view-mode-hook #'autocrypt-process-header))

(defun autocrypt-gnus-uninstall ()
  "Remove autocrypt hooks for Gnus."
  (remove-hook 'gnus-view-mode-hook #'autocrypt-process-header))

(defun autocrypt-gnus-header (field)
  "Ask Gnus to return header FIELD."
  (gnus-fetch-original-field field))

(provide 'autocrypt-gnus)

;;; autocrypt-gnus.el ends here
