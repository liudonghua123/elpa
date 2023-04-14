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
  (when url-http-oauth--registered-oauth-urls
    (let* ((url-no-query (url-parse-make-urlobj
                          (url-type url)
                          nil nil
                          (url-host url)
                          (url-portspec url)
                          (car (url-path-and-query url))
                          nil nil t))
           (key (url-http-oauth-url-string url-no-query)))
      (gethash key url-http-oauth--registered-oauth-urls))))

;; Maybe if RFC 8414, "OAuth 2.0 Authorization Server Metadata",
;; catches on, authorization-url and access-token-url can be made
;; optional and their values retrieved automatically.  As of early
;; 2023, RFC 8414 is not consistently implemented yet.
(defun url-http-oauth-register-resource (url
                                         authorization-url
                                         access-token-url
                                         client-identifier
                                         &optional
                                         client-secret-required)
  "Tell Emacs that to access URL, it needs to use OAuth 2.0.
URL will be accessed by Emacs's `url' library with a suitable
\"Authorization\" header containing \"Bearer <token>\".
AUTHORIZATION-URL and ACCESS-TOKEN-URL will be used to acquire
<token> and save it to the user's `auth-source' file.  URL,
AUTHORIZATION-URL and ACCESS-TOKEN-URL are either objects or
strings.  CLIENT-IDENTIFIER is a string identifying an Emacs
library or mode to the server.  CLIENT-SECRET-REQUIRED is the
symbol `prompt' if a client secret is required, nil otherwise."
  (unless url-http-oauth--registered-oauth-urls
    (setq url-http-oauth--registered-oauth-urls
          (make-hash-table :test #'equal)))
  (let ((key (url-http-oauth-url-string url))
        (authorization (url-http-oauth-url-string authorization-url))
        (access-token-object (url-http-oauth-url-object access-token-url)))
    (puthash key (list authorization access-token-object client-identifier
                       (cond
                        ((eq client-secret-required 'prompt) 'prompt)
                        ((eq client-secret-required nil) nil)
                        (t (error
                            "Unrecognized client-secret-required value"))))
             url-http-oauth--registered-oauth-urls)))

(defun url-http-oauth-unregister-resource (url)
  "Tell Emacs not to use OAuth 2.0 when accessing URL.
URL is either an objects or a string."
  (when url-http-oauth--registered-oauth-urls
    (remhash (url-http-oauth-url-string url)
             url-http-oauth--registered-oauth-urls)))

(defvar url-http-response-status)
(defvar auth-source-creation-prompts)

(defun url-http-oauth-port (url)
  "Return port of URL object.
Assume an HTTPS URL that does not specify a port uses 443."
  (or (url-port url) (when (string= "https" (url-type url)) 443)))

(defun url-http-oauth-get-access-token-grant (url code)
  "Get an access token for URL using CODE."
  (let* ((url-request-method "POST")
         (url-list (url-http-oauth-configuration url))
         (access-token-object (nth 1 url-list))
         (client-identifier (nth 2 url-list))
         (client-secret-required (nth 3 url-list))
         (auth-result
          (when client-secret-required
            (car (let ((auth-source-creation-prompts
                        '((secret . "Client secret for %u at %h")))
                       ;; Do not cache nil result.
                       (auth-source-do-cache nil))
                   (auth-source-search
                    :user client-identifier
                    :host (url-host access-token-object)
                    :port (url-http-oauth-port access-token-object)
                    :path (url-filename access-token-object)
                    :create '(path)
                    :max 1)))))
         (client-secret (auth-info-password auth-result))
         (save-function (plist-get auth-result :save-function))
         (authorization (concat
                         "Basic "
                         (base64-encode-string
                          (if client-secret
                              (format "%s:%s" client-identifier client-secret)
                            ;; FIXME: what to do if client-secret not required?
                            (format "%s" client-identifier))
                          t)))
         (url-request-extra-headers
          (list (cons "Content-Type" "application/x-www-form-urlencoded")
                (cons "Authorization" authorization)))
         (url-request-data
          (url-build-query-string
           (list (list "grant_type" "authorization_code")
                 (list "code" code)))))
    (with-current-buffer (url-retrieve-synchronously access-token-object)
      (if (eq 'OK (car (alist-get url-http-response-status url-http-codes)))
          (progn
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (let* ((grant (json-parse-buffer))
                   (type (gethash "token_type" grant)))
              (unless (equal type "bearer" )
                (error "Unrecognized token type %s for %s at %s" type
                       client-identifier (url-http-oauth-url-string url)))
              ;; Success, so save client secret, if necessary.
              (when (functionp save-function)
                (funcall save-function))
              ;; Return grant object.
              grant))
        (error "url-http-oauth: Failed to get access token with %s"
               (buffer-string))))))

(defun url-http-oauth-expiry-string (grant)
  "Return as a string a number representing the expiry time of GRANT.
The time is in seconds since the epoch."
  (format-time-string "%s" (time-add nil (gethash "expires_in" grant))))

(defun url-http-oauth-extract-authorization-code (url)
  "Extract the value of the code parameter in URL."
  (let ((query (cdr (url-path-and-query (url-generic-parse-url url)))))
    (unless query
      (error "url-http-oauth: Expected URL with query component"))
    (let ((code (cadr (assoc "code" (url-parse-query-string query)))))
      (unless code
        (error "url-http-oauth: Failed to find code in query component"))
      code)))

(defun url-http-oauth-get-bearer (url)
  "Prompt the user with the authorization endpoint for URL.
URL is a parsed object."
  (let* ((path-and-query (url-path-and-query url))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (scope (cadr (assoc "scope" (url-parse-query-string query))))
         (bearer-current (auth-info-password
                          (car
                           (let ((auth-source-do-cache nil))
                             (auth-source-search
                              :user (url-user url)
                              :host (url-host url)
                              :port (url-http-oauth-port url)
                              :path path
                              :scope scope
                              :max 1))))))
    (or bearer-current
        (let ((url-list (url-http-oauth-configuration url)))
          (unless url-list
            (error "%s is not registered with url-http-oauth"
                   (url-http-oauth-url-string url)))
          (let* ((response-url
                  (read-from-minibuffer
                   (format "Browse to %s and paste the redirected code URL: "
                           (concat (nth 0 url-list)
                                   "?"
                                   (url-build-query-string
                                    (list (list "client_id" (nth 2 url-list))
                                          (list "response_type" "code")
                                          (list "scope" scope)))))))
                 (code
                  (url-http-oauth-extract-authorization-code response-url)))
            (let* ((grant (url-http-oauth-get-access-token-grant url code))
                   (bearer-retrieved (gethash "access_token" grant))
                   (auth-result (let ((auth-source-do-cache nil))
                                  (auth-source-search
                                   :user (url-user url)
                                   :host (url-host url)
                                   :port (url-http-oauth-port url)
                                   :path path
                                   :scope (if (string= (gethash "scope" grant)
                                                       scope)
                                              scope
                                            (error
                                             (concat "url-http-oauth:"
                                                     " Returned scope did not"
                                                     " match requested scope")))
                                   :expiry (url-http-oauth-expiry-string grant)
                                   :secret bearer-retrieved
                                   :create '(path scope expiry)
                                   :max 1)))
                   (save-function (plist-get (car auth-result) :save-function)))
              ;; Success, so save bearer.
              (when (functionp save-function)
                (funcall save-function))
              bearer-retrieved))))))

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is an object representing a parsed URL.  It should specify a
user, and contain a \"scope\" query argument representing the
permissions that the caller is requesting."
  (when (url-http-oauth-configuration url)
    (let ((bearer (url-http-oauth-get-bearer url)))
      (concat "Bearer " bearer))))

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
