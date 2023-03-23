;; -*- lexical-binding: t; -*-
(require 'buildbot-client)
(require 'buildbot-view)

(define-derived-mode buildbot-build-mode buildbot-view-mode "Buildbot build"
  "Buildbot view for a build")

(defvar-local buildbot-build-build nil)
(defvar-local buildbot-build-revision-info nil)
(defun buildbot-build-buffer-name (buildid)
  (concat "*buildbot build " (number-to-string buildid) "*"))

(defun buildbot-build-load (build revision-info)
  (let ((buffer-name (buildbot-build-buffer-name (alist-get 'id build))))
    (with-current-buffer (get-buffer-create buffer-name)
      (buildbot-build-mode)
      (setq buildbot-build-build build
            buildbot-build-revision-info revision-info)
      (buildbot-build-update))
    (switch-to-buffer buffer-name)))

(defun buildbot-build-update ()
  (unless (derived-mode-p 'buildbot-build-mode)
    (error "Not in buildbot build mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((steps (buildbot-get-steps-by-buildid
                  (alist-get 'id buildbot-build-build))))
      (insert (buildbot-build-format
               buildbot-build-revision-info
               buildbot-build-build
               steps))
      (goto-char (point-min)))))

(defun buildbot-build-reload ()
  (interactive)
  (buildbot-build-update))
(define-key buildbot-build-mode-map "g" 'buildbot-build-reload)

(defun buildbot-build-format (revision-info build steps)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-step steps)
    "\n")))

(defun buildbot-build-open-step ()
  (interactive)
  (let ((step (get-text-property (point) 'step)))
    (unless step
      (error "Not at a step"))
    (buildbot-step-load buildbot-build-revision-info
                        buildbot-build-build
                        step)))
(define-key buildbot-build-mode-map (kbd "<return>")
  'buildbot-build-open-step)

(provide 'buildbot-build)
