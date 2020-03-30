;;; autocrypt-message.el --- Autocrypt for message-mode -*- lexical-binding:t -*-

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

;; MUA specific functions for `message-mode'
;;
;; Setup with (add-hook 'message-mode-hook #'autocrypt-mode)

;;; Code:

(require 'autocrypt "autocrypt")
(require 'message)

;;;###autoload
(defun autocrypt-message-install ()
  "Install autocrypt hooks for message mode."
  (add-hook 'message-setup-hook #'autocrypt-compose-setup)
  (add-hook 'message-send-hook #'autocrypt-compose-pre-send)
  (unless (lookup-key message-mode-map (kbd "C-c RET C-a"))
    (define-key message-mode-map (kbd "C-c RET C-a") #'autocrypt-compose-setup)))

(defun autocrypt-message-uninstall ()
  "Remove autocrypt hooks for message mode."
  (remove-hook 'message-setup-hook #'autocrypt-compose-setup)
  (remove-hook 'message-send-hook #'autocrypt-compose-pre-send)
  (when (eq (lookup-key message-mode-map (kbd "C-c RET C-a"))
            #'autocrypt-compose-setup)
    (define-key message-mode-map (kbd "C-c RET C-a") nil)))

(defun autocrypt-message-add-header (key val)
  "Insert header with key KEY and value VAL into message head."
  (message-add-header (concat key ": " val)))

(defun autocrypt-message-sign-encrypt ()
  "Sign and encrypt message."
  (mml-secure-message-sign-encrypt "pgpmime"))

(defun autocrypt-message-secure-attach (payload)
  "Attach and encrypt buffer PAYLOAD."
  (mml-attach-buffer payload)
  (mml-secure-part "pgpmime")
  (add-hook 'message-send-hook
            (lambda () (kill-buffer payload))
            nil t))

(provide 'autocrypt-message)

;;; autocrypt-message.el ends here
