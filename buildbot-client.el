;;; buildbot-client.el --- Client code using buildbot api -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.
;; 
;; This file is part of buildbot.el.
;; 
;; buildbot.el is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;; 
;; buildbot.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
;; details.
;; 
;; You should have received a copy of the GNU Affero General Public License
;; along with buildbot.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; buildbot-client provides functions for buildbot.el to get stuff from a
;; buildbot server.

;;; Code:
(require 'buildbot-utils)

(defvar buildbot-host)
(defvar buildbot-builders)

(defun buildbot-api-change (attr)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/changes?%s"
    buildbot-host (buildbot-format-attr attr))))

(defun buildbot-api-logs (stepid)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/steps/%d/logs"
    buildbot-host stepid)))

(defun buildbot-api-builders ()
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builders"
    buildbot-host)))

(defun buildbot-api-build (attr)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builds?%s"
    buildbot-host (buildbot-format-attr attr))))

(defun buildbot-api-step (buildid)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builds/%s/steps"
    buildbot-host buildid)))

(defun buildbot-api-log-raw (logid)
  (buildbot-url-fetch-raw
   (format "%s/api/v2/logs/%d/raw" buildbot-host logid)))

(defun buildbot-get-recent-changes (limit)
  (buildbot-api-change (list (cons 'order "-changeid") (cons 'limit limit))))

(defun buildbot-get-all-builders ()
  (alist-get 'builders (buildbot-api-builders)))

(defun buildbot-builder-by-id (builderid)
  (cl-find-if
   (lambda (builder)
     (= (alist-get 'builderid builder) builderid))
   buildbot-builders))

(defun buildbot-get-logs-by-stepid (stepid)
  (alist-get 'logs (buildbot-api-logs stepid)))

(defun buildbot-get-builder-name-by-id (id)
  (alist-get 'name (buildbot-builder-by-id id)))

(defun buildbot-get-changes-by-revision (revision)
  (alist-get 'changes
             (buildbot-api-change (list (cons 'revision revision)))))

(defun buildbot-get-build-by-buildid (buildid)
  (buildbot-api-build (list (cons 'buildid buildid))))

(defun buildbot-get-steps-by-buildid (buildid)
  (alist-get 'steps (buildbot-api-step buildid)))

(defun buildbot-get-changes-by-branch (branch-name limit)
  (alist-get 'changes
             (buildbot-api-change
              (cons `(branch . ,branch-name)
                    (when limit `((limit . ,limit)))))))

(provide 'buildbot-client)
