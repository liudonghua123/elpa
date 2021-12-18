;;; autocrypt-message.el --- Autocrypt for message-mode -*- lexical-binding:t -*-

;; Author: Philip Kaludercic <philipk@posteo.net>

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

(require 'message)
(require 'autocrypt)

;;;###autoload
(defun autocrypt-message--install ()
  "Prepare autocrypt for message buffers."
  (add-hook 'message-setup-hook #'autocrypt-compose-setup nil t)
  (add-hook 'message-send-hook #'autocrypt-compose-pre-send nil t)
  (unless (lookup-key message-mode-map (kbd "C-c RET C-a"))
    (define-key message-mode-map (kbd "C-c RET C-a") #'autocrypt-compose-setup)))

(defun autocrypt-message--uninstall ()
  "Remove autocrypt hooks for message mode."
  (remove-hook 'message-setup-hook #'autocrypt-compose-setup t)
  (remove-hook 'message-send-hook #'autocrypt-compose-pre-send t)
  (when (eq (lookup-key message-mode-map (kbd "C-c RET C-a"))
            #'autocrypt-compose-setup)
    (define-key message-mode-map (kbd "C-c RET C-a") nil)))

(defun autocrypt-message--get-header (header)
  "Return the value for HEADER."
  (message-fetch-field header))

(defun autocrypt-message--add-header (header value)
  "Insert HEADER with VALUE into the message head."
  (with-silent-modifications
    (message-add-header (concat header ": " value))))

(defun autocrypt-message--remove-header (header)
  "Insert HEADER with VALUE into the message head."
  (with-silent-modifications
    (message-remove-header header)))

(defun autocrypt-message--sign-encrypt ()
  "Sign and encrypt message."
  (mml-secure-message-sign-encrypt "pgpmime"))

(defun autocrypt-message--sign-secure-attach (payload)
  "Attach and encrypt buffer PAYLOAD."
  (mml-attach-buffer payload)
  (mml-secure-part "pgpmime")
  (add-hook 'message-send-hook
            (lambda () (kill-buffer payload))
            nil t))

(defun autocrypt-message--encrypted-p ()
  "Check if the current message is encrypted."
  (mml-secure-is-encrypted-p))

(provide 'autocrypt-message)

;;; autocrypt-message.el ends here
