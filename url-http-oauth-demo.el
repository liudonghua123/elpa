;;; url-http-oauth-demo.el --- Demo url-http-oauth -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 0
;; Keywords: comm, data, processes, hypermedia
;; Package-Requires: ((url-http-oauth "0"))

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
;; This package demonstrates an OAuth 2.0 flow for Sourcehut using the
;; built-in GNU Emacs URL library and the url-http-oauth package.
;;
;; Installation:
;;
;; M-x package-install RET url-http-oauth-demo RET
;;
;; Usage:
;;
;; M-x url-http-oauth-demo-get-profile-name RET
;; M-: (url-http-oauth-demo-get-profile-name) RET

;;; Code:
(require 'url-http-oauth)

;;; Register "https://meta.sr.ht/query" as needing OAuth 2.0 for
;;; authentication.
;;;###autoload
(url-http-oauth-register-resource "https://meta.sr.ht/query"
                                  "https://meta.sr.ht/oauth2/authorize"
                                  "https://meta.sr.ht/oauth2/access-token"
                                  "107ba4a9-2a96-4420-8818-84ec1f112405"
                                  'prompt)

;;;###autoload
(defun url-http-oauth-demo-get-profile-name ()
  "Asynchronously retrieve the Sourcehut profile name.
Print the result to *Messages*.  Return the name."
  (interactive)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (list (cons "Content-Type" "application/json")))
        (url-request-data
         "{\"query\": \"{ me { canonicalName } }\"}"))
    (with-current-buffer
        (url-retrieve-synchronously
         (url-parse-make-urlobj
          "https"             ; type
          "fitzsim"           ; user
          nil                 ; password, resolved by url-http-oauth
          "meta.sr.ht"        ; host
          443                 ; port
          (concat "/query"    ; path
                  "?"         ; scope:
                  (url-build-query-string
                   (list
                    (list "scope" "meta.sr.ht/PROFILE:RO"))))
          nil nil t))
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (message "%s" (buffer-substring (point) (point-max)))
      (gethash
       "canonicalName"
       (gethash
        "me"
        (gethash
         "data"
         (json-parse-buffer)))))))

(provide 'url-http-oauth-demo)

;;; url-http-oauth-demo.el ends here
