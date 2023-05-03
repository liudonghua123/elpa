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
;;
;; Usage:
;;
;; See url-http-oauth-demo.el.

;;; Code:
(require 'url-auth)
(require 'url-http)
(require 'url-util)
(require 'json)

(defvar url-http-oauth--interposed nil
  "A hash table mapping URL strings to lists of OAuth 2.0 settings.")

(defun url-http-oauth-url-string (url)
  "Ensure URL is a string."
  (if (stringp url) url (url-recreate-url url)))

(defun url-http-oauth-url-object (url)
  "Ensure URL is a parsed URL object."
  (if (stringp url) (url-generic-parse-url url) url))

(defun url-http-oauth-settings (url)
  "Return a settings list if URL needs OAuth 2.0, nil otherwise.
URL is either a URL object."
  (when url-http-oauth--interposed
    (let* ((url-no-query (url-parse-make-urlobj
                          (url-type url)
                          nil nil
                          (url-host url)
                          (url-portspec url)
                          (car (url-path-and-query url))
                          nil nil t))
           (key (url-http-oauth-url-string url-no-query)))
      (gethash key url-http-oauth--interposed))))

;; Maybe if RFC 8414, "OAuth 2.0 Authorization Server Metadata",
;; catches on, authorization-url and access-token-url can be made
;; optional and their values retrieved automatically.  As of early
;; 2023, RFC 8414 is not consistently implemented yet.
(defun url-http-oauth-interpose (url-settings)
  "Arrange for Emacs to use OAuth 2.0 to access a URL using URL-SETTINGS.
URL-SETTINGS is an alist with fields whose descriptions follow.
URL will be accessed by Emacs's `url' library with a suitable
\"Authorization\" header containing \"Bearer <token>\".
AUTHORIZATION-URL and ACCESS-TOKEN-URL will be used to acquire
<token> and save it to the user's `auth-source' file.  URL,
AUTHORIZATION-URL and ACCESS-TOKEN-URL are either objects or
strings.  CLIENT-IDENTIFIER is a string identifying an Emacs
library or mode to the server.  SCOPE is a string defining the
-permissions that the Emacs library or mode is requesting.
CLIENT-SECRET-METHOD is the symbol `prompt' if a client secret is
required, nil otherwise."
  (unless url-http-oauth--interposed
    (setq url-http-oauth--interposed (make-hash-table :test #'equal)))
  (let* ((urls (cdr (assoc "urls" url-settings)))
         (client-secret-method
          (cdr (assoc "client-secret-method" url-settings))))
    (unless (or (eq client-secret-method 'prompt) (eq client-secret-method nil))
      (error "Unrecognized client-secret-method value"))
    (dolist (url urls)
      (puthash (url-http-oauth-url-string url) url-settings
               url-http-oauth--interposed))))

(defun url-http-oauth-uninterpose (url-settings)
  "Arrange for Emacs not to use OAuth 2.0 when accessing URL in URL-SETTINGS.
This function does the opposite of `url-http-oauth-interpose'."
  (when url-http-oauth--interposed
    (let* ((urls (cdr (assoc "urls" url-settings))))
      (dolist (url urls)
        (remhash (url-http-oauth-url-string url)
                 url-http-oauth--interposed)))))

(defvar url-http-response-status)
(defvar auth-source-creation-prompts)
;; FIXME: if anything goes wrong during the authentication steps,
;; `url-http-end-of-document-sentinel' calls back into
;; `url-oauth-auth' somehow.  Maybe `url-http-no-retry' can help here?
(defvar url-http-no-retry)

(defun url-http-oauth-port (url)
  "Return port of URL object.
Assume an HTTPS URL that does not specify a port uses 443."
  (let ((port-number (url-port url)))
    (if port-number
        (number-to-string port-number)
      (when (string= "https" (url-type url)) "443"))))

(defun url-http-oauth-auth-source-search (&rest spec)
  "Like `auth-source-search' but search for all of SPEC in all backends.
Filter out nil spec entries prior to searching."
  (let* ((auth-source-do-cache nil) ; do not cache nil result.
         (all (apply #'auth-source-search :max 5001 spec)) ; hmm, no :max 'all.
         (spec (cl-loop for i below (length spec) by 2
                        unless (null (nth (1+ i) spec))
                        collect (nth i spec)
                        unless (null (nth (1+ i) spec))
                        collect (nth (1+ i) spec)))
         (result (cl-loop for entry in all
                          when (auth-source-specmatchp spec entry)
                          collect entry)))
    (unless (or (eq (length result) 0)
                (eq (length result) 1))
      (warn "url-http-oauth-auth-source-search produced multiple results for %s"
            spec))
    result))

(defun url-http-oauth-encode-scope (scope)
  "Replace spaces in SCOPE with plus signs."
  (replace-regexp-in-string " " "+" scope))

;; Backport of `auth-info-password'.
(defun url-http-oauth-auth-info-password (auth-info)
  "Return the :secret password from the AUTH-INFO."
  (let ((secret (plist-get auth-info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

;; Backport (roughly) of `json-parse-buffer'.
(defun url-http-oauth-json-parse-buffer ()
  "See `json-parse-buffer'."
  (let ((json-object-type 'hash-table))
    (json-read-from-string
     (buffer-substring (point) (point-max)))))

(defun url-http-oauth-get-access-token-grant (url code)
  "Get an access token for URL using CODE."
  (let* ((url-request-method "POST")
         (url-settings (url-http-oauth-settings url))
         (access-token-object
          (url-http-oauth-url-object
           (cdr (assoc "access-token-endpoint" url-settings))))
         (client-identifier (cdr (assoc "client-identifier" url-settings)))
         (scope (cdr (assoc "scope" url-settings)))
         (client-secret-method (cdr (assoc "client-secret-method"
                                           url-settings)))
         (auth-result
          (when client-secret-method
            (car (let* ((auth-source-creation-prompts
                         '((secret . "Client secret for %u at %h: ")))
                        (spec (list :user client-identifier
                                    :host (url-host access-token-object)
                                    :port (url-http-oauth-port
                                           access-token-object)
                                    :path (url-filename access-token-object)
                                    :scope
                                    (url-http-oauth-encode-scope scope))))
                   (or (apply #'url-http-oauth-auth-source-search spec)
                       (apply #'auth-source-search :create '(path scope) spec))))))
         (client-secret (url-http-oauth-auth-info-password auth-result))
         (save-function (plist-get auth-result :save-function))
         (authorization (when client-secret
                          (concat
                           "Basic "
                           (base64-encode-string
                            (format "%s:%s" client-identifier client-secret)
                            t))))
         (url-request-extra-headers
          (apply #'list
                 (cons "Content-Type" "application/x-www-form-urlencoded")
                 (when authorization (cons "Authorization" authorization))))
         (redirect-uri
          (cdr (assoc "redirect_uri"
                      (cdr (assoc "authorization-extra-arguments"
                                  url-settings)))))
         (url-request-data
          (url-build-query-string
           (apply #'list (list "code" code)
                  (list "client_id" client-identifier)
                  (list "grant_type" "authorization_code")
                  (when redirect-uri
                    (list (list "redirect_uri" redirect-uri)))))))
    (with-current-buffer (url-retrieve-synchronously access-token-object)
      (if (eq 'OK (car (alist-get url-http-response-status url-http-codes)))
          (progn
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (let* ((grant (url-http-oauth-json-parse-buffer))
                   (type (gethash "token_type" grant)))
              (unless (equal (dowcase type) "bearer")
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

(defun url-http-oauth-authorization-url (url-settings)
  "Return the authorization URL for URL-SETTINGS."
  (let ((base (cdr (assoc "authorization-endpoint" url-settings)))
        (client
         (list "client_id" (cdr (assoc "client-identifier" url-settings))))
        (response-type (list "response_type" "code"))
        (scope (list "scope" (cdr (assoc "scope" url-settings))))
        (extra (mapcar (lambda (entry)
                         (list (car entry) (cdr entry)))
                (cdr (assoc "authorization-extra-arguments" url-settings)))))
    (concat base "?" (url-build-query-string
                      (apply #'list client response-type scope extra)))))

(defun url-http-oauth-get-bearer (url)
  "Prompt the user with the authorization endpoint for URL.
URL is a parsed object."
  (let* ((url (url-http-oauth-url-object url))
         (url-settings (url-http-oauth-settings url))
         (path-and-query (url-path-and-query url))
         (path (car path-and-query))
         (scope (url-http-oauth-encode-scope (cdr (assoc "scope" url-settings))))
         (bearer-current (url-http-oauth-auth-info-password
                          (car
                           (let ((auth-source-do-cache nil))
                             (url-http-oauth-auth-source-search
                              :user "BEARER"
                              :host (url-host url)
                              :port (url-http-oauth-port url)
                              :path path
                              :scope scope))))))
    (unless url-settings
      (error "%s is not interposed by url-http-oauth"
             (url-http-oauth-url-string url)))
    (or bearer-current
        (let* ((response-url
                (read-from-minibuffer
                 (format "Browse to %s and paste the redirected code URL: "
                         (url-http-oauth-authorization-url url-settings))))
               (code
                (url-http-oauth-extract-authorization-code response-url))
               (grant (url-http-oauth-get-access-token-grant url code))
               (bearer-retrieved (gethash "access_token" grant))
               (auth-result (auth-source-search
                             :create '(path scope expiry)
                             ;; If :user is nil, then
                             ;; (auth-source-search :create ...) will
                             ;; find the client-identifier username.
                             ;; :user isn't used for bearer tokens
                             ;; anyway, so use this dummy name to
                             ;; differentiate the bearer token
                             ;; authinfo line from the
                             ;; client-identifier/client-secret
                             ;; authinfo line.
                             :user "BEARER"
                             :host (url-host url)
                             :port (url-http-oauth-port url)
                             :path path
                             :scope
                             (let ((returned-scope
                                    (gethash "scope" grant)))
                               (if (string=
                                    (url-http-oauth-encode-scope
                                     returned-scope)
                                    scope)
                                   scope
                                 (error
                                  (concat "url-http-oauth:"
                                          " Returned scope %S did not"
                                          " match requested scope"
                                          returned-scope))))
                             :expiry (url-http-oauth-expiry-string grant)
                             :secret bearer-retrieved))
               (save-function (plist-get (car auth-result) :save-function)))
          ;; Success; save bearer.
          (when (functionp save-function)
            (funcall save-function))
          bearer-retrieved))))

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is an object representing a parsed URL.  It should specify a
user, and contain a \"scope\" query argument representing the
permissions that the caller is requesting."
  (when (url-http-oauth-settings url)
    (let ((bearer (url-http-oauth-get-bearer url)))
      (concat "Bearer " bearer))))

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
