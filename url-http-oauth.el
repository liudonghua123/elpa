;;; url-http-oauth.el --- Add OAuth 2.0 authentication support to URL library -*- lexical-binding: t -*-

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
;; This package provides an OAuth 2.0 handler for Emacs's URL library.
;;
;; Installation:
;;
;; M-x package-install RET url-http-oauth RET

;;; Code:
(require 'url-auth)
(require 'url-http)
(require 'url-util)

;; For evaluation during development:
;; (setq url-http-oauth--registered-oauth-urls nil)
;; (message "%S" url-http-oauth--registered-oauth-urls)
(defvar url-http-oauth--registered-oauth-urls nil
  "A hash table mapping URL strings to lists of OAuth 2.0 configuration.")

(defun url-http-oauth-url-string (url)
  "Ensure URL is a string."
  (if (url-p url) (url-recreate-url url) url))

(defun url-http-oauth-url-object (url)
  "Ensure URL is a parsed URL object."
  (if (url-p url) url (url-generic-parse-url url)))

;; Maybe if RFC 8414, "OAuth 2.0 Authorization Server Metadata",
;; catches on, authorize-url and access-token-url can be made
;; optional, and their values retrieved automatically.  But from what
;; I can tell RFC 8414 is not consistently implemented yet.
(defun url-http-oauth-register-provider (url authorize-url access-token-url
                                             client-identifier scope)
  "Register URL as an OAuth 2.0 provider.
URL will be accessed by Emacs with a suitable \"Authorization\"
header containing \"Bearer <token>\".  AUTHORIZE-URL and
ACCESS-TOKEN-URL will be used to acquire <token> and save it to
the user's `auth-source' file.  URL and ACCESS-TOKEN-URL are
either URL structures or URL strings."
  (unless url-http-oauth--registered-oauth-urls
    (setq url-http-oauth--registered-oauth-urls
          (make-hash-table :test #'equal)))
  (let ((key (url-http-oauth-url-string url))
        (authorize (url-http-oauth-url-string authorize-url))
        (access-token (url-http-oauth-url-string access-token-url)))
    (puthash key (list authorize access-token client-identifier scope)
             url-http-oauth--registered-oauth-urls)))


(defun url-http-oauth-get-access-token (url code)
  "Get an access token for URL using CODE."
  (let* ((url-request-method "POST")
         (key-url (url-http-oauth-url-string url))
         (url-list (gethash key-url url-http-oauth--registered-oauth-urls))
         (access-token-url (nth 1 url-list))
         (client-identifier (nth 2 url-list))
         (client-secret
          (auth-info-password (car (auth-source-search :host access-token-url
                                                       :user client-identifier
                                                       :max 1))))
         (authorization (concat "Basic "
                                (base64-encode-string
                                 (format "%s:%s" client-identifier
                                         client-secret)
                                 t)))
	 (url-request-extra-headers
	  (list (cons "Content-Type" "application/x-www-form-urlencoded")
                (cons "Authorization" authorization)))
	 (url-request-data
	  (mm-url-encode-www-form-urlencoded
           (list (cons "grant_type" "authorization_code")
                 (cons "code" code)))))
    (url-retrieve access-token-url
                  (lambda (status arguments)
                    (let ((event (plist-get status :error)))
                      (if event
                          (error "Failed to get token: %s" event)
                        (goto-char (point-min))
                        (re-search-forward "\n\n")
                        (let* ((grant (json-parse-buffer))
                               (type (gethash "token_type" grant)))
                          (unless (equal type "bearer" )
                            (error "Unrecognized token type: %s" type))
                          (auth-source-search :host key-url
                                              :secret (gethash "access_token")
                                              :expiry (gethash "expires_in")
                                              :create t))))))))

;; FIXME: why doesn't the authentication get saved?
;; (funcall (plist-get (car (auth-source-search :host "https://meta.sr.ht/query" :secret "example" :expiry 86399 :create t)) :save-function))
(defun url-http-oauth-extract-authorization-code (url)
  "Extract the value of the code parameter in URL."
  (let ((filename (url-filename (url-generic-parse-url url)))
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

(defun url-http-oauth-get-authorization-code (url)
  "Prompt the user with the authorization endpoint for URL."
  (let* ((key-url (url-http-oauth-url-string url))
         (url-list
          (gethash key-url url-http-oauth--registered-oauth-urls)))
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
                                    (cons "scope" (nth 3 url-list))))))))
           (code (url-http-oauth-extract-authorization-code response-url)))
      (url-http-oauth-get-access-token url code))))

(defvar url-http-oauth-testval nil "Test value.")
(setq url-http-oauth-testval nil)
(setq url-http-oauth-testval (url-http-oauth-authorize "https://meta.sr.ht/query"))

;; works: (auth-source-search :max 1 :host "https://meta.sr.ht/oauth2/access-token")
(defvar url-http-oauth-fulltokenbuf nil "Test buf.")
(setq url-http-oauth-fulltokenbuf
      (url-http-oauth-get-access-token "https://meta.sr.ht/query" "eb869898585b6e21cf016dc0126d48e8"))

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is a structure representing a parsed URL."
  ;; Do nothing for now.
  (when url nil))

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
