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
(defun url-http-oauth-register-provider (url authorize-url access-token-url)
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
        (authorize (url-http-oauth-url-object authorize-url))
        (access-token (url-http-oauth-url-object access-token-url)))
    (puthash key (list authorize access-token)
             url-http-oauth--registered-oauth-urls)))

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
