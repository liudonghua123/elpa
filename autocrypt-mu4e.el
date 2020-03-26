;;; autocrypt-mu4e.el --- Autocrypt for mu4e -*- lexical-binding:t -*-

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

;; MUA specific functions for mu4e
;;
;; Setup with (add-hook 'mu4e-main-mode-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt)
(require 'mu4e)

;;;###autoload
(defun autocrypt-mu4e-install ()
  "Install autocrypt hooks for mu4e."
  (add-hook 'mu4e-view-mode-hook #'autocrypt-process-header))

(defun autocrypt-mu4e-uninstall ()
  "Remove autocrypt hooks for mu4e."
  (remove-hook 'mu4e-view-mode-hook #'autocrypt-process-header))

(defun autocrypt-mu4e-header (field)
  "Ask mu4e to return header FIELD."
  (save-window-excursion
    (with-current-buffer (mu4e-view-raw-message)
      (prog1 (mail-fetch-field field)
        (kill-buffer (current-buffer))))))

(provide 'autocrypt-mu4e)

;;; autocrypt-mu4e.el ends here
