;;; autocrypt-mu4e.el --- Autocrypt for mu4e -*- lexical-binding:t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>

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

(declare-function mu4e-view-raw-message "mu4e" ())

;;;###autocrypt
(defun autocrypt-mu4e--install ()
  "Install autocrypt hooks for mu4e."
  (add-hook (if (boundp 'mu4e-view-rendered-hook)
                'mu4e-view-rendered-hook
              'mu4e-view-mode-hook)
            #'autocrypt-process-header nil t)
  (add-hook 'mu4e-compose-mode-hook #'autocrypt-compose-setup nil t))

(defun autocrypt-mu4e--uninstall ()
  "Remove autocrypt hooks for mu4e."
  (remove-hook 'mu4e-view-mode-hook #'autocrypt-process-header t)
  (remove-hook 'mu4e-compose-mode-hook #'autocrypt-compose-setup t))

(defun autocrypt-mu4e--get-header (header)
  "Ask mu4e to return HEADER."
  (save-window-excursion
    (with-current-buffer (mu4e-view-raw-message)
      (prog1 (mail-fetch-field header)
        (kill-buffer (current-buffer))))))

(provide 'autocrypt-mu4e)

;;; autocrypt-mu4e.el ends here
