;;; autocrypt.el --- Autocrypt implementation -*- lexical-binding:t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.4.0
;; Keywords: comm
;; Package-Requires: ((emacs "24.1"))
;; URL: https://git.sr.ht/~zge/autocrypt

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:

;; Implementation of Autocrypt (https://autocrypt.org/) for various
;; Emacs MUAs.

;; Run M-x `autocrypt-create-account' to initialise an autocrypt key,
;; and add `autocrypt-mode' to your MUA's hooks (`gnus-mode-hook',
;; `message-mode-hook', ...) to activate it's usage.

;;; Code:

(require 'message)
(require 'cl-lib)
(require 'rx)

 ;; CUSTOMIZABLES

(defgroup autocrypt nil
  "Autocrypt protocol implementation for Emacs MUAs"
  :tag "Autocrypt"
  :group 'mail
  :link '(url-link "https://autocrypt.org/")
  :version "28.1"
  :prefix "autocrypt-")

(defcustom autocrypt-accounts nil
  "Alist of supported Autocrypt accounts.

All elements have the form (MAIL FINGERPRINT PREFERENCE), where
FINGERPRINT is the fingerprint of the PGP key that should be used
by email address MAIL. PREFERENCE must be one of `mutual' or
`no-preference', `none' (if no preference should be inserted into
headers), or nil if this account should be temporarily disabled.

This variable doesn't have to be manually specified, as
activating the command `autocrypt-mode' should automatically
configure it, or by calling `autocrypt-create-account'."
  :type '(alist :key-type string
                :value-type
                (group (string :tag "Fingerprint")
                       (choice :tag "Encryption Preference"
                               (const :tag "None" none)
                               (const :tag "No Preference" no-preference)
                               (const :tag "Mutual" mutual)
                               (const :tag "Disable this Account" nil)))))

(defcustom autocrypt-do-gossip t
  "Enable Autocrypt gossiping.

This will inject \"Autocrypt-Gossip\" headers when required, and
process \"Autocrypt-Gossip\" headers when received."
  :type '(choice (const :tag "Enable Gossip" t)
                 (const :tag "Only receive" 'only-receive)
                 (const :tag "Only send" 'only-send)
                 (const :tag "Disable Gossip" nil)))

(defcustom autocrypt-save-file
  (expand-file-name "autocrypt-data.el" user-emacs-directory)
  "File where Autocrypt peer data should be saved."
  :type '(file :must-match t))

 ;; DATA STRUCTURES

;; https://autocrypt.org/level1.html#communication-peers
(cl-defstruct autocrypt-peer
  last-seen
  timestamp
  pubkey
  preference
  gossip-timestamp
  gossip-key
  deactivated)

 ;; INTERNAL STATE

(defvar autocrypt-peers nil
  "List of known autocrypt peers.

Every member of this list has to be an instance of the
`autocrypt-peer' structure.")

(defconst autocrypt-save-variables '(autocrypt-peers)
  "List of variables to save to `autocrypt-save-data'.")

 ;; MUA TRANSLATION LAYER

(defconst autocrypt-mua-func-alist
  '((gnus
     :install autocrypt-gnus-install
     :uninstall autocrypt-gnus-uninstall
     :header autocrypt-gnus-header)
    (rmail
     :install autocrypt-rmail-install
     :uninstall autocrypt-rmail-uninstall
     :header autocrypt-rmail-header)
    (mu4e
     :install autocrypt-mu4e-install
     :uninstall autocrypt-mu4e-uninstall
     :header autocrypt-mu4e-header)
    (message
     :install autocrypt-message-install
     :uninstall autocrypt-message-uninstall))
  "Alist for all MUA specific functions.

The value of each record is a plist. The value of each property
is the symbol of the function that should be called, when
refereed to by the property (see `autocrypt-mua-call').

Valid properties and their associated messages are:

:install - called when `autocrypt-mode' is activated in the
current buffer. Should only install hooks.

:uninstall - inverse of :install, and should reverse it's
effects, ie. usually removing hooks.

:header - a MUA viewer (Gnus, Rmail, ...) must provide such a
message to let autocrypt query headers. The function must accept
one argument, and return the value as a string.

:part - a MUA viewer might provide such a message to let
autocrypt query parts. The function must accept a index (as
in IDX'th part) and return either a buffer, string or
cons-cell of the form (file . PATH), where PATH points to where
the part contents can be found.")

(defsubst autocrypt-get-mua ()
  "Return key for MUA based on major mode.

The key should identify a record in the
`autocrypt-mua-func-alist' alist."
  (cond
   ((derived-mode-p 'gnus-mode)
    'gnus)
   ((derived-mode-p 'rmail-mode)
    'rmail)
   ((derived-mode-p 'message-mode)
    'message)
   (t (user-error "MUA not supported"))))

(defsubst autocrypt-mua-func (msg)
  "Return function behind MSG for major mode."
  (plist-get (cdr (assq (autocrypt-get-mua)
                        autocrypt-mua-func-alist))
             msg))

(defun autocrypt-mua-call (msg &rest args)
  "Call function behind MSG with ARGS for major mode."
  (let ((func (autocrypt-mua-func msg)))
    (and func (apply func args))))

 ;; INTERNAL FUNCTIONS

;; https://autocrypt.org/level1.html#e-mail-address-canonicalization
(defsubst autocrypt-canonicalise (addr)
  "Return a canonical form of email address ADDR."
  ;; "[...] Other canonicalization efforts are considered for later
  ;; specification versions."
  (save-match-data
    (let ((parts (mail-extract-address-components addr)))
      (downcase (cadr parts)))))

(defun autocrypt-load-data ()
  "Load peer data if exists from `autocrypt-save-file'."
  (when (file-exists-p autocrypt-save-file)
    (load autocrypt-save-file t t t)))

(defun autocrypt-save-data ()
  "Write peer data save-file to `autocrypt-save-file'."
  (with-temp-buffer
    (insert ";; generated by autocrypt.el.\n"
            ";; do not modify by hand\n")
    (let ((standard-output (current-buffer)))
      (dolist (var autocrypt-save-variables)
        (print
         `(unless ,var
            (setq ,var ',(symbol-value var))))))
    (write-file autocrypt-save-file)))

;; https://autocrypt.org/level1.html#recommendations-for-single-recipient-messages
(defun autocrypt-single-recommentation (sender recipient)
  "Calculate autocrypt recommendation for a single RECIPIENT.

SENDER is a string containing the value of the From field."
  (let ((self (cdr (assoc (autocrypt-canonicalise sender) autocrypt-accounts)))
        (peer (cdr (assoc (autocrypt-canonicalise recipient) autocrypt-peers))))
    ;; TODO: check if keys are revoked, expired or unusable
    (cond
     ((not (and peer (autocrypt-peer-pubkey peer)))
      'disabled)
     ((and (eq (autocrypt-peer-preference peer) 'mutual)
           (eq (cadr self) 'mutual))
      'encrypt)
     ((or (eq (autocrypt-peer-preference peer) 'mutual)
          (> (- (time-to-days (current-time))
                (time-to-days (autocrypt-peer-last-seen peer)))
             35))
      'discourage)
     (t 'available))))

;; https://autocrypt.org/level1.html#provide-a-recommendation-for-message-encryption
(defun autocrypt-recommendation (sender recipients)
  "Calculate a autocrypt recommendation for RECIPIENTS.

SENDER is a string containing the value of the From field."
  (let ((res (mapcar (apply-partially #'autocrypt-single-recommentation sender)
                     recipients)))
    (cond
     ((or (null res) (memq 'disabled res)) 'disabled)
     ((memq 'discourage res) 'discourage)
     ((null (delq 'encrypt res)) 'encrypt)
     (t 'available))))

;; https://autocrypt.org/level1.html#the-autocrypt-header
(defun autocrypt-parse-header (string)
  "Destruct Autocrypt header from STRING.

Returns list with address, preference and key data if
well-formed, otherwise returns just nil."
  (with-temp-buffer
    (save-excursion
      (insert string))
    (let (addr pref keydata)
      (save-match-data
        (while (looking-at (rx (group (+ (any alnum ?_ ?-)))
                               (* space) "=" (* space)
                               (group (* (not (any ";"))))
                               (? ";" (* space))))
          (cond
           ((string= (match-string 1) "addr")
            (setq addr (autocrypt-canonicalise (match-string 2))))
           ((string= (match-string 1) "prefer-encrypt")
            (setq pref (cond
                        ((string= (match-string 2) "mutual")
                         'mutual)
                        ((string= (match-string 2) "nopreference")
                         'no-preference))))
           ((string= (match-string 1) "keydata")
            (setq keydata (base64-decode-string (match-string 2))))
           ((string-prefix-p "_" (match-string 1)) ; ignore
            nil)
           (t (error "Unsupported Autocrypt key")))
          (goto-char (match-end 0))))
      (and addr keydata (list addr pref keydata)))))

;;; https://autocrypt.org/level1.html#updating-autocrypt-peer-state-from-key-gossip
(defun autocrypt-process-gossip (date)
  "Update internal autocrypt gossip state."
  (let ((recip (autocrypt-list-recipients))
        (root (autocrypt-mua-call :part 0))
        (re (rx bol "Autocrypt-Gossip:" (* space)
                (group (+ (or nonl (: "\n "))))
                eol))
        gossip)
    (when root
      (catch 'unsupported
        (with-temp-buffer
          ;; The MUA interface should have the flexibility to return
          ;; different kinds of part containers. Currently
          ;; buffers, strings and a cons-cell of the form '(file . PATH)
          ;; are supported, where PATH is a string describing where to
          ;; find the part.
          (cond
           ((bufferp root)
            (insert (with-current-buffer root (buffer-string))))
           ((stringp root)
            (insert root))
           ((eq (car root) 'file)
            (insert-file-contents (cdr root)))
           (t
            ;; If unsupported, then the gossip processing should fail
            ;; SILENTLY. Autocrypt should not annoy the user.
            (throw 'unsupported nil)))
          (ietf-drums-narrow-to-header)
          (goto-char (point-min))
          (save-match-data
            (while (search-forward-regexp re nil t)
              (push (autocrypt-parse-header (match-string 1))
                    gossip))))
        (dolist (datum (delq nil gossip))
          (let* ((addr (car datum))
                 (peer (cdr (assoc addr recip))))
            (if (and peer (time-less-p (autocrypt-peer-gossip-timestamp peer)
                                       date))
                (setf (autocrypt-peer-gossip-timestamp date)
                      (autocrypt-peer-gossip-key (caddr datum)))
              (push (cons addr (make-autocrypt-peer
                                :gossip-timestamp date
                                :gossip-key (caddr datum)))
                    autocrypt-peers))))))))

;; https://autocrypt.org/level1.html#updating-autocrypt-peer-state
(defun autocrypt-process-header ()
  "Update internal autocrypt state."
  (let* ((from (autocrypt-canonicalise (autocrypt-mua-call :header "From")))
         (date (mail-header-parse-date (autocrypt-mua-call :header "Date")))
         (header (autocrypt-mua-call :header "Autocrypt"))
         parse addr preference keydata peer)
    (when header
      (when (setq parse (autocrypt-parse-header header))
        (setq addr (autocrypt-canonicalise (car parse))
              preference (cadr parse)
              keydata (caddr parse)
              peer (or (cdr (assoc addr autocrypt-peers))
                       (make-autocrypt-peer
                        :last-seen date
                        :timestamp date
                        :pubkey keydata
                        :preference preference)))))
    (when autocrypt-do-gossip
      (autocrypt-process-gossip date))
    (when (string= from addr)
      (unless (time-less-p date (autocrypt-peer-timestamp peer))
        (when (time-less-p (autocrypt-peer-last-seen peer) date)
          (setf (autocrypt-peer-last-seen peer) date))
        (if keydata                         ; has "Autocrypt" header
            (setf (autocrypt-peer-preference peer) (or preference 'none)
                  (autocrypt-peer-deactivated peer) nil
                  (autocrypt-peer-timestamp peer) date
                  (autocrypt-peer-pubkey peer) keydata)
          (setf (autocrypt-peer-deactivated peer) t))
        (unless (assoc addr autocrypt-peers)
          (push (cons addr peer) autocrypt-peers))))))

(defun autocrypt-insert-keydata (data)
  "Insert raw keydata DATA as base64 at point."
  (let ((start (point)))
    (insert data)
    (base64-encode-region start (point))
    (save-restriction
      (narrow-to-region start (point))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (insert " ")
          (forward-line))))))

(defun autocrypt-get-keydata (acc)
  "Generate key for ACC to insert into header."
  (let* ((ctx (epg-make-context))
         (keys (epg-list-keys ctx (cadr acc) 'secret)))
    (epg-export-keys-to-string ctx keys)))

(defun autocrypt-generate-header (addr &optional gossip-p)
  "Generate header value for address ADDR.

If ADDR is a local account, it's key will be used. Otherwise it
attempts to look up ADDR in the peer data. If nothing was found
OR the header is too large, return nil."
  (let (acc peer pref keydata)
    (cond
     ((setq acc (assoc addr autocrypt-accounts))
      (setq keydata (autocrypt-get-keydata acc)
            pref (cadr acc)))
     ((setq peer (assoc addr autocrypt-peers))
      (setq keydata (autocrypt-peer-pubkey peer)
            pref (autocrypt-peer-preference peer))))
    (when (and (not (and peer (autocrypt-peer-deactivated peer)))
               pref keydata)
      (with-temp-buffer
        (insert "addr=" addr "; ")
        (cond
         (gossip-p
          ;; Key Gossip Injection [...] SHOULD NOT include a
          ;; prefer-encrypt attribute.
          nil)
         ((eq pref 'no-preference)
          (insert "prefer-encrypt=nopreference; "))
         ((eq pref 'mutual)
          (insert "prefer-encrypt=mutual; ")))
        (insert "keydata=")
        (autocrypt-insert-keydata keydata)
        (and (< (buffer-size) (* 10 1024))
             (buffer-string))))))

(defun autocrypt-create-account (name email expire pass)
  "Create a GPG key for Autocrypt."
  (interactive (list (read-string "Name: " user-full-name)
                     (read-string "Email: " user-mail-address)
                     (read-string "Expire Date (date or <n>{d,w,m,y}): ")
                     (let ((prompt "Passphrase (must be non-empty): ") pass)
                       (while (eq (setq pass (read-passwd prompt t t)) t))
                       pass)))
  (let ((ctx (epg-make-context)))
    (epg-generate-key-from-string
     ctx
     (with-temp-buffer
       ;; https://www.gnupg.org/documentation/manuals/gnupg/Unattended-GPG-key-generation.html
       ;; https://lists.gnupg.org/pipermail/gnupg-users/2017-December/059622.html
       (insert "Key-Type: eddsa\n")
       (insert "Key-Curve: Ed25519\n")
       (insert "Key-Usage: sign\n")
       (insert "Subkey-Type: ecdh\n")
       (insert "Subkey-Curve: Curve25519\n")
       (insert "Subkey-Usage: encrypt\n")
       (insert "Name-Real: " name "\n")
       (insert "Name-Email: " email "\n")
       (insert "Name-Comment: generated by autocrypt.el\n")
       (insert "Passphrase: " pass "\n")
       (insert "Expire-Date: " (if (string= "" expire) "0" expire) "\n")
       (insert "%commit")
       (buffer-string)))
    (let ((res (epg-context-result-for ctx 'generate-key)))
      (unless res
        (error "Could not determine fingerprint"))
      (customize-set-variable
       'autocrypt-accounts
       (cons (list email (cdr (assq 'fingerprint (car res))) 'none)
             autocrypt-accounts)
       "Customized by autocrypt.el"))
    (message "Successfully generated key for %s, and added to key chain."
             email)))

 ;; GNUS SUPPORT

(with-eval-after-load 'gnus
  ;; setup with (add-hook 'gnus-load-hook #'autocrypt-mode)

  (defun autocrypt-gnus-install ()
    "Install autocrypt hooks for Gnus."
    (add-hook 'gnus-view-mode-hook #'autocrypt-process-header))

  (defun autocrypt-gnus-uninstall ()
    "Remove autocrypt hooks for Gnus."
    (remove-hook 'gnus-view-mode-hook #'autocrypt-process-header))

  (defun autocrypt-gnus-header (field)
    "Ask Gnus to return header FIELD."
    (gnus-fetch-original-field field)))

 ;; RMAIL SUPPORT

(with-eval-after-load 'rmail
  ;; setup with (add-hook 'rmail-mode-hook #'autocrypt-mode)

  (defun autocrypt-rmail-install ()
    "Install autocrypt hooks for Rmail."
    (add-hook 'rmail-show-message-hook #'autocrypt-process-header))

  (defun autocrypt-rmail-uninstall ()
    "Remove autocrypt hooks for Rmail."
    (remove-hook 'rmail-show-message-hook #'autocrypt-process-header))

  (defun autocrypt-rmail-header (field)
    "Ask Rmail to return header field."
    (rmail-apply-in-message
     rmail-current-message
     (lambda () (mail-fetch-field field)))))

 ;; MU4E SUPPORT

(with-eval-after-load 'mu4e
  ;; setup with (advice-add 'mu4e :after (lambda (&rest _) (autocrypt-mode)))

  (defun autocrypt-mu4e-install ()
    "Install autocrypt hooks for mu4e."
    (add-hook 'mu4e-view-mode-hook #'autocrypt-process-header))

  (defun autocrypt-mu4e-uninstall ()
    "Remove autocrypt hooks for mu4e."
    (remove-hook 'mu4e-view-mode-hook #'autocrypt-process-header))

  (defun autocrypt-mu4e-header (field)
    "Ask mu4e to return header field."
    (save-window-excursion
      (with-current-buffer (mu4e-view-raw-message)
        (prog1 (mail-fetch-field field)
          (kill-buffer (current-buffer)))))))

 ;; MESSAGE SUPPORT

(with-eval-after-load 'message
  ;; setup with (add-hook 'message-mode-hook #'autocrypt-mode)

  (defun autocrypt-message-install ()
    "Install autocrypt hooks for message-mode."
    (add-hook 'message-setup-hook #'autocrypt-message-setup)
    (add-hook 'message-send-hook #'autocrypt-message-pre-send)
    (define-key message-mode-map (kbd "C-c RET C-a") #'autocrypt-message-setup))

  (defun autocrypt-message-uninstall ()
    "Remove autocrypt hooks for message-mode."
    (remove-hook 'message-setup-hook #'autocrypt-message-setup)
    (remove-hook 'message-send-hook #'autocrypt-message-pre-send)
    (define-key message-mode-map (kbd "C-c RET C-a") nil))

  (defun autocrypt-message-list-recipients ()
    "Return a list of all recipients to this message."
    (let (recipients)
      (dolist (hdr '("To" "Cc" "Reply-To"))
        (let* ((f (message-field-value hdr))
               (r (and f (mail-extract-address-components f t))))
          (setq recipients (nconc (mapcar #'cadr r) recipients))))
      (delete-dups recipients)))

  ;; https://autocrypt.org/level1.html#key-gossip-injection-in-outbound-messages
  (defun autocrypt-message-gossip-p (recipients)
    "Find out if the current message should have gossip headers."
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
    (let ((recs (autocrypt-message-list-recipients))
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
    (let* ((recs (autocrypt-message-list-recipients))
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
            (dolist (addr (autocrypt-message-list-recipients))
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
          (message-add-header (concat "Autocrypt: " header)))))))

 ;; MINOR MODES

;;;###autoload
(define-minor-mode autocrypt-mode
  "Enable Autocrypt support in current buffer.

Behaviour shall adapt to current major mode. Should be added to
the startup hook of your preferred MUA or mail-related major
mode."
  :group 'autocrypt
  (if autocrypt-mode
      (progn
        (add-hook 'kill-emacs-hook #'autocrypt-save-data)
        (autocrypt-load-data)
        (autocrypt-mua-call :install))
    (autocrypt-save-data)
    (autocrypt-mua-call :uninstall)))

(provide 'autocrypt)

;;; autocrypt.el ends here
