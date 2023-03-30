;;; buildbot-view.el --- buildbot.el UI -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.
;; 
;; This file is part of buildbot.el.
;; 
;; buildbot.el is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; buildbot.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with buildbot.el.  If not, see <https://www.gnu.org/licenses/>.

(require 'buildbot-utils)
(require 'buildbot-client)

(defvar buildbot-view-header-regex "^\\[.*\\]$")
(defvar buildbot-view-branch-change-limit 10)
(defvar buildbot-view-builder-build-limit 50)
;; 'revision, 'build, 'step, or 'log
(defvar-local buildbot-view-type nil)
(defvar-local buildbot-view-data nil)

(define-derived-mode buildbot-view-mode special-mode "Buildbot"
  "buildbot.el is a Buildbot client for emacs.")

(defun buildbot-view-next-header (n)
  (interactive "p")
  (dotimes (_ n)
    (end-of-line 1)
    (re-search-forward buildbot-view-header-regex)
    (beginning-of-line 1)))
(define-key buildbot-view-mode-map "n" 'buildbot-view-next-header)

(defun buildbot-view-next-header-same-thing (n)
  (interactive "p")
  (when-let
      ((type (get-text-property (point) 'type)))
    (dotimes (_ n)
      (buildbot-view-next-header 1)
      (while (not (eq (get-text-property (point) 'type) type))
        (buildbot-view-next-header 1)))))
(define-key buildbot-view-mode-map (kbd "M-n")
  'buildbot-view-next-header-same-thing)

(defun buildbot-view-previous-header (n)
  (interactive "p")
  (beginning-of-line 1)
  (unless (looking-at buildbot-view-header-regex)
    (re-search-backward buildbot-view-header-regex))
  (dotimes (_ n)
    (re-search-backward buildbot-view-header-regex)))
(define-key buildbot-view-mode-map "p" 'buildbot-view-previous-header)

(defun buildbot-view-previous-header-same-thing (n)
  (interactive "p")
  (when-let
      ((type (get-text-property (point) 'type)))
    (dotimes (_ n)
      (buildbot-view-previous-header 1)
      (while (not (eq (get-text-property (point) 'type) type))
        (buildbot-view-previous-header 1)))))
(define-key buildbot-view-mode-map (kbd "M-p")
  'buildbot-view-previous-header-same-thing)

(defun buildbot-view-format-revision-info (revision-info)
  (propertize
   (format
    "[Revision %s]\nAuthor: %s\nDate: %s\n\n%s"
    (alist-get 'revision revision-info)
    (alist-get 'author revision-info)
    (alist-get 'created-at revision-info)
    (alist-get 'comments revision-info))
   'revision (alist-get 'revision revision-info) 'type 'revision))

(defun buildbot-view-format-build-stats (stats)
  (format "Build stats: Success - %d | Failure - %d | Pending - %d"
          (alist-get 'success stats)
          (alist-get 'failure stats)
          (alist-get 'pending stats)))

(defun buildbot-view-format-build (revision build &optional show-revision)
  (propertize
   (format "\n[%s | %s]\n%s"
           (if show-revision
               revision
             (buildbot-get-builder-name-by-id (alist-get 'builderid build)))
           (propertize (alist-get 'state_string build)
                       'face (buildbot-status-face
                              (buildbot-build-status build)))
           (string-join
            (mapcar (lambda (test) (alist-get 'test_name test))
                    (alist-get 'failed_tests build))
            "\n"))
    'revision revision 'build build 'type 'build))

(defun buildbot-view-format-change-info (change-info &optional no-branch)
  (let ((revision (alist-get 'revision change-info)))
    (concat
     (unless no-branch
       (concat (buildbot-view-format-branch (alist-get 'branch change-info))
               "\n"))
     (buildbot-view-format-build-stats (alist-get 'build-stats change-info))
     "\n"
     (string-join
      (mapcar
       (lambda (build)
         (buildbot-view-format-build revision build))
       (alist-get 'builds change-info))
      "\n"))))

(defun buildbot-view-format-step (step)
  (propertize
   (format "\n[%d. %s | %s]\n"
           (alist-get 'number step)
           (alist-get 'name step)
           (propertize
            (alist-get 'state_string step)
            'face (buildbot-status-face
                   (buildbot-step-guess-status step))))
   'step step 'type 'step))

(defun buildbot-view-format-log (log)
  (propertize
   (format "\n[%s]\n"
           (alist-get 'name log))
   'log log 'type 'log))

(defun buildbot-revision-format (revision-and-changes-info &optional no-branch)
  (let ((revision-info (alist-get 'revision-info revision-and-changes-info)))
    (concat
     (buildbot-view-format-revision-info revision-info)
     "\n\n"
     (string-join
      (mapcar (lambda (change-info)
                (buildbot-view-format-change-info change-info no-branch))
              (alist-get 'changes-info revision-and-changes-info))
      "\n"))))

(defun buildbot-view-format-branch (branch)
  (propertize
   (format "[Branch %s]" branch)
   'branch branch
   'type 'branch))

(defun buildbot-branch-format (branch changes)
  (concat
   (buildbot-view-format-branch branch)
   "\n\n"
   (string-join
    (mapcar (lambda (change)
              (buildbot-revision-format
               (buildbot-get-revision-and-changes-info (list change))
               t))
            changes)
    "\n\n")))

(defun buildbot-view-format-builder (builder)
  (propertize
   (format "[Builder %s]" (alist-get 'name builder))
   'builder builder 'type 'builder))

(defun buildbot-builder-format (builder builds-with-revisions)
  (concat
   (buildbot-view-format-builder builder)
   "\n\n"
   (string-join
    (mapcar
     (lambda (build-with-revision)
       (buildbot-view-format-build
        (elt (alist-get 'revision
                        (alist-get 'properties build-with-revision))
             0)
        (assq-delete-all 'properties build-with-revision)
        t))
     builds-with-revisions)
    "\n\n")))

(defun buildbot-build-format (revision-info build steps)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build (alist-get 'revision revision-info) build)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-step steps)
    "\n")))

(defun buildbot-step-format (revision-info build step logs)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-log logs)
    "\n")))

(defun buildbot-log-format (revision-info build step log log-text)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (buildbot-view-format-log log)
   "\n"
   log-text))

(defun buildbot-get-id-from-build (build)
  (or (alist-get 'id build)
      (alist-get 'buildid build)))

(defun buildbot-view-buffer-name (type data)
  (pcase type
    ('branch (format "*buildbot branch %s*" (alist-get 'branch data)))
    ('revision (format "*buildbot revision %s*"
                       (alist-get 'revision data)))
    ('builder (format "*buildbot builder %s*"
                      (alist-get 'name
                                 (alist-get 'builder data))))
    ('build (format "*buildbot build %d*"
                    (buildbot-get-id-from-build
                     (alist-get 'build data))))
    ('step (format "*buildbot step %d*"
                   (alist-get 'stepid (alist-get 'step data))))
    ('log (format "*buildbot log %d*"
                  (alist-get 'logid (alist-get 'log data))))))

(defun buildbot-view-open (type data &optional force)
  (let ((buffer-name (buildbot-view-buffer-name type data)))
    (when (or force (not (get-buffer buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (buildbot-view-mode)
        (setq buildbot-view-type type
              buildbot-view-data data)
        (buildbot-view-update)))
    (switch-to-buffer buffer-name)))

(defun buildbot-view-reload ()
  (interactive)
  (buildbot-view-update))
(define-key buildbot-view-mode-map "g" 'buildbot-view-reload)

;;;###autoload
(defun buildbot-revision-open (revision)
  (interactive "sRevision (e.g. commit hash): ")
  (buildbot-view-open 'revision `((revision . ,revision))))

;;;###autoload
(defun buildbot-branch-open (branch)
  (interactive "sBranch name: ")
  (buildbot-view-open 'branch `((branch . ,branch))))

;;;###autoload
(defun buildbot-builder-open (builder-name)
  (interactive (list (completing-read
                      "Builder name: "
                      (mapcar
                       (lambda (builder) (alist-get 'name builder))
                       buildbot-builders))))
  (buildbot-view-open 'builder
                      (list (cons 'builder
                                  (buildbot-builder-by-name builder-name)))))

(defun buildbot-view-update ()
  (unless (derived-mode-p 'buildbot-view-mode)
    (error "Not in buildbot view mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pcase buildbot-view-type
      ('branch
       (insert (buildbot-branch-format
                (alist-get 'branch buildbot-view-data)
                (buildbot-get-changes-by-branch
                 (alist-get 'branch buildbot-view-data)
                 buildbot-view-branch-change-limit))))
      ('revision
       (let ((revision-and-changes-info
              (buildbot-get-revision-and-changes-info
               (buildbot-get-changes-by-revision
                (alist-get 'revision buildbot-view-data)))))
         (setf (alist-get 'revision-info buildbot-view-data)
               (alist-get 'revision-info revision-and-changes-info))
         (insert (buildbot-revision-format revision-and-changes-info))))
      ('builder
       (let* ((builder (alist-get 'builder buildbot-view-data))
              (builds
               (buildbot-get-recent-builds-by-builder
                (alist-get 'builderid builder)
                buildbot-view-builder-build-limit)))
         (insert (buildbot-builder-format builder builds))))
      ('build
       (let ((revision (alist-get 'revision buildbot-view-data)))
         (unless (equal (alist-get 'revision
                                   (alist-get 'revision-info buildbot-view-data))
                        revision)
           (setf (alist-get 'revision-info buildbot-view-data)
                 (buildbot-get-revision-info-from-change
                  (elt
                   (buildbot-get-changes-by-revision revision)
                   0)))))
       (insert (buildbot-build-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (buildbot-get-steps-by-buildid
                 (buildbot-get-id-from-build
                  (alist-get 'build buildbot-view-data))))))
      ('step
       (insert (buildbot-step-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (buildbot-get-logs-by-stepid
                 (alist-get 'stepid
                            (alist-get 'step buildbot-view-data))))))
      ('log
       (insert (buildbot-log-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (alist-get 'log buildbot-view-data)
                (buildbot-api-log-raw
                 (alist-get 'logid
                            (alist-get 'log buildbot-view-data)))))))
    (goto-char (point-min))))

(defun buildbot-view-open-thing-at-point (force)
  (interactive "P")
  (let ((data (copy-tree buildbot-view-data)))
    (pcase (get-text-property (point) 'type)
      ('branch
       (setf (alist-get 'branch data)
             (get-text-property (point) 'branch))
       (buildbot-view-open 'branch data force))
      ('revision
       (setf (alist-get 'revision data)
             (get-text-property (point) 'revision))
       (buildbot-view-open 'revision data force))
      ('build
       (setf (alist-get 'build data)
             (get-text-property (point) 'build)
             (alist-get 'revision data)
             (get-text-property (point) 'revision))
       (buildbot-view-open 'build data force))
      ('step
       (setf (alist-get 'step data)
             (get-text-property (point) 'step))
       (buildbot-view-open 'step data force))
      ('log
       (setf (alist-get 'log data)
             (get-text-property (point) 'log))
       (buildbot-view-open 'log data force)))))
(define-key buildbot-view-mode-map (kbd "<return>")
  'buildbot-view-open-thing-at-point)

(provide 'buildbot-view)
