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

(defvar url-http-oauth--registered-oauth-urls nil
  "A hash table mapping URL strings to lists of OAuth 2.0 configuration items.")

;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-oauth-auth (url &optional _prompt _overwrite _realm _args)
  "Return an OAuth 2.0 HTTP authorization header.
URL is a structure representing a parsed URL."
  nil)

;;; Register `url-oauth-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "oauth" nil 9)

(provide 'url-http-oauth)

;;; url-http-oauth.el ends here
