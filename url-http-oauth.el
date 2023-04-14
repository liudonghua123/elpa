;;; url-http-oauth.el --- OAuth 2.0 for URL library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 0
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package adds OAuth 2.0 support to Emacs's URL library.
;;
;; Installation:
;;
;; M-x package-install RET url-http-oauth RET

;;; Code:
(require 'url-auth)
(require 'url-http)
(require 'url-util)
(require 'mm-url)

;; For evaluation during development:
;; (setq url-http-oauth--registered-oauth-urls nil)
;; (message "%S" url-http-oauth--registered-oauth-urls)
(defvar url-http-oauth--registered-oauth-urls nil
  "A hash table mapping URL strings to lists of OAuth 2.0 configuration.")

(defun url-http-oauth-url-string (url)
  "Ensure URL is a string."
  (if (stringp url) url (url-recreate-url url)))

(defun url-http-oauth-url-object (url)
  "Ensure URL is a parsed URL object."
  (if (stringp url) (url-generic-parse-url url) url))

(defun url-http-oauth-configuration (url)
  "Return a configuration list if URL needs OAuth 2.0, nil otherwise.
URL is either a URL object or a URL string."
  (let ((key (url-http-oauth-url-string url)))
    (gethash key url-http-oauth--registered-oauth-urls)))

;; Maybe if RFC 8414, "OAuth 2.0 Authorization Server Metadata",
;; catches on, authorization-url and access-token-url can be made
;; optional, and their values retrieved automatically.  But from what
;; I can tell RFC 8414 is not consistently implemented yet.
(defun url-http-oauth-register-resource (url
                                         authorization-url
                                         access-token-url
                                         client-identifier
                                         scope
                                         &optional
                                         client-secret-required)
  "Tell Emacs that to access URL, it needs to use OAuth 2.0.
URL will be accessed by Emacs's `url' library with a suitable
\"Authorization\" header containing \"Bearer <token>\".
AUTHORIZATION-URL and ACCESS-TOKEN-URL will be used to acquire
<token> and save it to the user's `auth-source' file.  URL,
AUTHORIZATION-URL and ACCESS-TOKEN-URL are either URL objects or
URL strings.  CLIENT-IDENTIFIER is a string identifying an Emacs
library or mode to the server.  SCOPE is a string defining the
permissions that the Emacs library or mode is requesting.
CLIENT-SECRET-REQUIRED is the symbol `prompt' if a client secret
is required, nil otherwise."
  (unless url-http-oauth--registered-oauth-urls
    (setq url-http-oauth--registered-oauth-urls
          (make-hash-table :test #'equal)))
  (let ((key (url-http-oauth-url-string url))
        (authorization (url-http-oauth-url-string authorization-url))
        (access-token (url-http-oauth-url-string access-token-url)))
    (puthash key (list authorization access-token client-identifier scope
                       (cond
                        ((eq client-secret-required 'prompt) 'prompt)
                        ((eq client-secret-required nil) nil)
                        (t (error
                            "Unrecognized client-secret-required value"))))
             url-http-oauth--registered-oauth-urls)))

;; (car (auth-source-search :host "https://meta.sr.ht/oauth2/access-token" :login "107ba4a9-2a96-4420-8818-84ec1f112405" :max 1))

(defvar url-http-response-status)

(defun url-http-oauth-get-access-token (url code)
  "Get an access token for URL using CODE."
  (let* ((url-request-method "POST")
         (key-url (url-http-oauth-url-string url))
         (url-list (url-http-oauth-configuration key-url))
         (access-token-url (nth 1 url-list))
         (client-identifier (nth 2 url-list))
         (client-secret-required (nth 4 url-list))
         (client-secret-current (when client-secret-required
                                  (auth-info-password
                                   (car (auth-source-search
                                         :host access-token-url
                                         ;; FIXME: Why doesn't :user
                                         ;; work here, but :login
                                         ;; does?
                                         :login client-identifier
                                         :max 1)))))
         (client-secret-read (unless client-secret-current
                               (when client-secret-required
                                 (read-from-minibuffer
                                  (format "Client secret for %s at %s: "
                                          client-identifier key-url)))))
         (authorization (concat
                         "Basic "
                         (base64-encode-string
                          (format "%s:%s" client-identifier
                                  (or client-secret-current client-secret-read
                                      ;; FIXME what to do if not required?
                                      ""))
                          t)))
         (url-request-extra-headers
          (list (cons "Content-Type" "application/x-www-form-urlencoded")
                (cons "Authorization" authorization)))
         (url-request-data
          (mm-url-encode-www-form-urlencoded
           (list (cons "grant_type" "authorization_code")
                 (cons "code" code)))))
    ;; (url-retrieve access-token-url
    ;;               (lambda (status)
    ;;                 (let ((event (plist-get status :error)))
    ;;                   (if event
    ;;                       (error "Failed to get token: %s" event)
    ;;                     (goto-char (point-min))
    ;;                     (re-search-forward "\n\n")
    ;;                     (let* ((grant (json-parse-buffer))
    ;;                            (type (gethash "token_type" grant)))
    ;;                       (unless (equal type "bearer" )
    ;;                         (error "Unrecognized token type: %s" type))
    ;;                       (auth-source-search
    ;;                        :host key-url
    ;;                        :secret (gethash "access_token" grant)
    ;;                        :expiry (gethash "expires_in" grant)
    ;;                        :create t))))))
    (with-current-buffer (url-retrieve-synchronously access-token-url)
      (if (eq 'OK (car (alist-get url-http-response-status url-http-codes)))
          (progn
            (message "BUFFER-STRING: %s" (buffer-string)) ; FIXME: remove after testing.
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (let* ((grant (json-parse-buffer))
                   (type (gethash "token_type" grant)))
              (message "GRANT: %S" grant) ; FIXME: remove after testing.
              (unless (equal type "bearer" )
                (error "Unrecognized token type %s for %s at %s" type
                       client-identifier key-url))
              ;; Success, so save client secret, if necessary.
              (when (and (not client-secret-current)
                         client-secret-read)
                (let* ((auth-result (auth-source-search
                                     :host access-token-url
                                     ;; FIXME: Why does :user here get
                                     ;; translated to "login" in
                                     ;; authinfo.gpg?
                                     :user client-identifier
                                     :secret client-secret-read
                                     :create t))
                       (save-function (plist-get (car auth-result)
                                                 :save-function)))
                  (if (functionp save-function)
                      (funcall save-function)
                    (warn "Saving client secret for %s at %s failed"
                          client-identifier key-url))))
              ;; Return access token string.
              (gethash "access_token" grant)))
        (error "url-http-oauth: Failed to get access token with %s"
               (buffer-string))))))

(defun url-http-oauth-extract-authorization-code (url)
  "Extract the value of the code parameter in URL."
  (let* ((filename (url-filename (url-generic-parse-url url)))
         (query-index (string-search "?" filename)))
    (unless query-index
      (error "Expected a URL with a query component after a `?' character"))
    (let* ((query (substring filename (1+ query-index)))
           (code
            (catch 'found
              (dolist (parameter (string-split query "&" t))
                (let ((pair (split-string parameter "=")))
                  (when (equal (car pair) "code")
                    (throw 'found (cadr pair))))))))
      (unless code
        (error "Could not find code in pasted URL"))
      code)))

(defun url-http-oauth-get-bearer (url)
  "Prompt the user with the authorization endpoint for URL."
  (let* ((key-url (url-http-oauth-url-string url))
         (url-list (url-http-oauth-configuration key-url)))
    (unless url-list
      (error "%s is not registered with url-http-oauth" key-url))
    (let* ((response-url
            (read-from-minibuffer
             (format "Browse to %s and paste the redirected code URL: "
                     (concat (nth 0 url-list)
                             "?"
                             (mm-url-encode-www-form-urlencoded
                              (list (cons "client_id" (nth 2 url-list))
                                    (cons "response_type" "code")
                                    ;; FIXME: expiry?
                                    (cons "scope" (nth 3 url-list))))))))
           (code (url-http-oauth-extract-authorization-code response-url)))
      (url-http-oauth-get-access-token url code)
      ;; FIXME: why doesn't the authentication get saved?
      ;; (funcall (plist-get (car (auth-source-search :host "https://meta.sr.ht/query" :secret "example" :expiry 86399 :create t)) :save-function))
      )))

;; (setq fitzsim-banana (auth-source-search :host "banana" :secret "orange3" :create t))

;; Works, but need 
;; (when (functionp (plist-get (car fitzsim-banana) :save-function)) (funcall (plist-get (car fitzsim-banana) :save-function)))

;;(defvar url-http-oauth-testval nil "Test value.")
;;(setq url-http-oauth-testval nil)
;;(setq url-http-oauth-testval (url-http-oauth-get-authorization-code "https://meta.sr.ht/query"))

;; works: (auth-source-search :max 1 :host "https://meta.sr.ht/oauth2/access-token")
;; (defvar url-http-oauth-fulltokenbuf nil "Test buf.")
;; (setq url-http-oauth-fulltokenbuf (url-http-oauth-get-access-token "https://meta.sr.ht/query" "eb869898585b6e21cf016dc0126d48e8"))

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is an object representing a parsed URL."
  ;; Do nothing for now.
  (when (url-http-oauth-configuration url)
    (let ((bearer (url-http-oauth-get-bearer url)))
      (message "BEARER: %s" bearer)
      (concat "Bearer " bearer))))

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
