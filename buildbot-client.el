;; -*- lexical-binding: t; -*-
(require 'buildbot-utils)

(defun buildbot-api-change (attr)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/changes?%s"
    buildbot-host (buildbot-format-attr attr))))

(defun buildbot-api-logs (stepid)
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/steps/%s/logs"
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

(defun buildbot-format-log-url (logid)
  (format "%s/api/v2/logs/%s/raw" buildbot-host logid))

(defun buildbot-api-recent-changes (limit)
  (buildbot-api-change (list (cons 'order "-changeid") (cons 'limit limit))))

(defun buildbot-get-all-builders ()
  (alist-get 'builders (buildbot-api-builders)))

(defun buildbot-builder-by-id (builderid)
  (cl-find-if
   (lambda (builder)
     (= (alist-get 'builderid builder) builderid))
   buildbot-builders))

(defun buildbot-get-builder-name-by-id (id)
  (alist-get 'name (buildbot-builder-by-id id)))

(defun buildbot-get-change-by-revision (revision)
  (elt
   (alist-get 'changes
              (buildbot-api-change (list (cons 'revision revision))))
   0))

(defun buildbot-get-build-by-buildid (buildid)
  (buildbot-api-build (list (cons 'buildid buildid))))

(defun buildbot-get-builds-by-revision (revision)
  (alist-get 'builds (buildbot-get-change-by-revision revision)))

(defun buildbot-get-failed-builds-by-revision (revision)
  (seq-filter
   (lambda (build)
     (not (equal (alist-get 'state_string build) "build successful")))
   (buildbot-get-builds-by-revision revision)))

(defun buildbot-format-builds-by-revision (revision)
  (mapcar
   'buildbot-format-build
   (buildbot-get-builds-by-revision revision)))

(defun buildbot-get-steps-by-buildid (buildid)
  (alist-get 'steps (buildbot-api-step buildid)))

(defun buildbot-get-logs-by-stepid (stepid)
  (alist-get 'logs (buildbot-api-logs stepid)))

(provide 'buildbot-client)
