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

(require 'autocrypt)
(require 'message)

(defun autocrypt-message-install ()
  "Install autocrypt hooks for message mode."
  (add-hook 'message-setup-hook #'autocrypt-message-setup)
  (add-hook 'message-send-hook #'autocrypt-message-pre-send)
  (define-key message-mode-map (kbd "C-c RET C-a") #'autocrypt-message-setup))

(defun autocrypt-message-uninstall ()
  "Remove autocrypt hooks for message mode."
  (remove-hook 'message-setup-hook #'autocrypt-message-setup)
  (remove-hook 'message-send-hook #'autocrypt-message-pre-send)
  (define-key message-mode-map (kbd "C-c RET C-a") nil))

;; https://autocrypt.org/level1.html#key-gossip-injection-in-outbound-messages
(defun autocrypt-message-gossip-p (recipients)
  "Find out if the current message should have gossip headers.
Argument RECIPIENTS is a list of addresses this message is
addressed to."
  (and (mml-secure-is-encrypted-p)
       (< 1 (length recipients))
       (cl-every
        (lambda (rec)
          (let ((peer (cdr (assoc rec autocrypt-peers))))
            (and peer (not (autocrypt-peer-deactivated peer)))))
        recipients)))

(defun autocrypt-message-setup ()
  "Check if Autocrypt is possible, and add pseudo headers."
  (interactive)
  (let ((recs (autocrypt-list-recipients))
        (from (autocrypt-canonicalise (message-field-value "from"))))
    ;; encrypt message if applicable
    (save-excursion
      (cl-case (autocrypt-recommendation from recs)
        (available
         (message-add-header "Do-Autocrypt: no"))
        (discourage
         (message-add-header "Do-Discouraged-Autocrypt: no"))))))

(defun autocrypt-message-pre-send ()
  "Insert Autocrypt headers before sending a message.

Will handle and remove \"Do-(Discourage-)Autocrypt\" if found."
  (let* ((recs (autocrypt-list-recipients))
         (from (autocrypt-canonicalise (message-field-value "from"))))
    ;; encrypt message if applicable
    (when (eq (autocrypt-recommendation from recs) 'encrypt)
      (mml-secure-message-sign-encrypt "pgpmime"))
    ;; check for manual autocrypt confirmations
    (let ((do-autocrypt (message-fetch-field "Do-Autocrypt"))
          (ddo-autocrypt (message-fetch-field "Do-Discouraged-Autocrypt"))
          (query "Are you sure you want to use Autocrypt, even though it is discouraged?"))
      (when (and (not (mml-secure-is-encrypted-p))
                 (or (and do-autocrypt
                          (string= (downcase do-autocrypt) "yes"))
                     (and ddo-autocrypt
                          (string= (downcase ddo-autocrypt) "yes")
                          (yes-or-no-p query))))
        (mml-secure-message-sign-encrypt "pgpmime")))
    (message-remove-header "Do-Autocrypt")
    (message-remove-header "Do-Discouraged-Autocrypt")
    ;; insert gossip data
    (when (autocrypt-message-gossip-p recs)
      (let ((buf (generate-new-buffer " *autocrypt gossip*")))
        (with-current-buffer buf
          (dolist (addr (autocrypt-list-recipients))
            (let ((header (autocrypt-generate-header addr t)))
              (insert "Autocrypt-Gossip: " header "\n"))))
        (mml-attach-buffer buf)
        (mml-secure-part "pgpmime")
        (add-hook 'message-send-hook
                  (lambda () (kill-buffer buf))
                  nil t)))
    ;; insert autocrypt header
    (let ((header (and from (autocrypt-generate-header from))))
      (when header
        (message-add-header (concat "Autocrypt: " header))))))

(provide 'autocrypt-message)

;;; autocrypt-message.el ends here
