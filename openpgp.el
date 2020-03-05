;;; $Id: openpgp.el,v 1.13 2020/03/05 13:30:16 oj14ozun Exp oj14ozun $
;;; Implementation of the keys.openpgp.org protocol as specified by
;;; https://keys.openpgp.org/about/api

(require 'json)
(require 'url)
(require 'url-http)
(require 'epg)

(defcustom openpgp-keyserver "keys.openpgp.org"
  "Domain of keyserver to use.

NOTE: currently the default value is the only working keyserver,
as federation hasn't been implemented yet.")

 ;; FETCHING KEYS

(defsubst openpgp--api-url (endpoint &optional arg)
  "Construct VKS request querying ENDPOINT.

The optional argument ARG will be concatenated to the end of the
URL, if non-nil."
  (format "https://%s/vks/v1/%s%s"
	  openpgp-keyserver endpoint
	  (if arg (concat "/" arg) "")))

(defun openpgp--process-key (status)
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (forward-line)
  (epa-import-armor-in-region (point) (point-max)))

(defun openpgp-fetch-key-by-fingerprint (fingerprint)
  "Query key via FINGERPRINT and add to keychain."
  (let ((fingerprint (string-remove-prefix "0X" (upcase fingerprint))))
    (url-retrieve (openpgp--api-url "by-fingerprint" fingerprint)
		  #'openpgp--process-key)))

(defun openpgp-fetch-key-by-keyid (keyid)
  "Query key via KEYID and add to keychain."
  (let ((keyid (string-remove-prefix "0X" (upcase keyid))))
    (url-retrieve (openpgp--api-url "by-keyid" keyid)
		  #'openpgp--process-key)))

(defun openpgp-fetch-key-by-email (email)
  "Query key via EMAIL and add to keychain."
  (url-retrieve (openpgp--api-url "by-email" (url-hexify-string email))
		#'openpgp--process-key))

 ;; UPLOADING KEYS

(defun openpgp--key-ids ()
  (let* ((keys (epg-list-keys (epg-make-context)))
	 (uids (mapcan #'epg-key-user-id-list keys))
	 (ids (mapcar #'epg-user-id-string uids))
	 (addr (mapcar #'mail-extract-address-components ids)))
    (mapcan (lambda (addr) (and (car addr) (cdr addr))) addr)))

(defun openpgp--verify-callback (status email)
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (let* ((json-object-type 'hash-table)
	 (data (json-read)))
    (when (gethash "error" data)
      (error "Error in response: %s" (gethash "error" data)))
    (let ((resp (gethash email (gethash "status" data))))
      (cond ((null resp)
	     (message "Verification request might have failed. Are you using the correct address?"))
	    ((string= resp "unpublished")
	     (message "Verification request might have failed. Are you using the correct key?"))
	    ((string= resp "published")
	     (message "Verification request succeeded, but key has already been published."))
	    ((string= resp "revoked")
	     (message "Verification request succeeded, but key has been revoked."))
	    ((string= resp " pending")
	     (message "Verification request succeeded, a email should arrive soon."))))))

(defun openpgp-request-verify (email token)
  "Request verification email for address EMAIL.

TOKEN should be supplied by a previous \"upload-key\" request."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data (json-encode `(("token" . ,token)
					 ("addresses" . (,email))))))
    (url-retrieve (openpgp--api-url "request-verify")
		  #'openpgp--verify-callback
		  (list email))))

(defun openpgp--upload-callback (status email)
  (when (plist-get status :error)
    (error "Request failed: %s"
	   (caddr (assq (caddr (plist-get status :error))
			url-http-codes))))
  (forward-paragraph)
  (let* ((json-object-type 'hash-table)
	 (data (json-read)))
    (when (gethash "error" data)
      (error "Error in response: %s" (gethash "error" data)))
    (let ((resp (gethash email (gethash "status" data))))
      (when (cond ((null resp)
		   (yes-or-no-p "Your address hasn't been recognised by the server, are you sure you want to proceed?"))
		  ((string= resp "published")
		   (yes-or-no-p "Key has already been published, are you sure you want to proceed?"))
		  ((string= resp "revoked")
		   (yes-or-no-p "Key has been revoked, are you sure you want to proceed?"))
		  ((string= resp " pending")
		   (yes-or-no-p "Key is already pending, are you sure you want to proceed?"))
		  (t t))
	(openpgp-request-verify email (cdr (assq 'token data)))))))

(defun openpgp-upload-key-string (email key)
  "Upload KEY for address EMAIL to keyserver.

The KEY should be a string, containing a ASCII armoured public
key."
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/json")))
	(url-request-data (json-encode `(("keytext" . ,key)))))
    (url-retrieve (openpgp--api-url "upload")
		  #'openpgp--upload-callback
		  (list email))))

(defun openpgp-upload-key-file (email key-file)
  "Upload key from KEY-FILE for address EMAIL."
  (interactive (list (completing-read "Email: " (openpgp--key-ids)
				      nil nil nil nil user-mail-address)
		     (read-file-name "Key file: ")))
  (with-temp-buffer
    (insert-file-contents key-file)
    (openpgp-upload-key-string email (buffer-string))))

(defun openpgp-upload-key (email)
  "Upload public key for address EMAIL using gpg."
  (interactive (list (completing-read "Email: " (openpgp--key-ids)
				      nil nil nil nil user-mail-address)))
  (let* ((addr (shell-quote-argument email))
	 (cmd (format "gpg --armor --export %s" addr))
	 (out (shell-command-to-string cmd)))
    (openpgp-upload-key-string email out)))

 ;; MAIL CLIENT SUPPORT

(with-eval-after-load 'rmail
  (defun openpgp-rmail-fetch-key ()
    "Fetch key for the sender of the current message."
    (interactive)
    (when (or (null rmail-current-message)
	      (zerop rmail-current-message))
      (error "There is no message to fetch a key for"))
    (let ((email (or (mail-fetch-field "mail-reply-to" nil t)
		     (mail-fetch-field "reply-to" nil t)
		     (mail-fetch-field "from"))))
      (when (yes-or-no-p (format "Attempt to fetch key for %s? " email))
	(openpgp-fetch-key-by-email email)))))

(with-eval-after-load 'mu4e
  (defun openpgp-mu4e-fetch-key ()
    "Fetch key for the sender of the current message."
    (interactive)
	(let ((msg (mu4e-message-at-point 'noerror)))
     (unless msg
       (error "There is no message to fetch a key for")))
    (let ((email (or (mu4e-message-field msg :reply-to)
		     (mu4e-message-field msg :from))))
      (when (yes-or-no-p (format "Attempt to fetch key for %s? " email))
	(openpgp-fetch-key-by-email email)))))
