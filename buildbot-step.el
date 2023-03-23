;; -*- lexical-binding: t; -*-
(require 'buildbot-client)
(require 'buildbot-view)

(define-derived-mode buildbot-step-mode buildbot-view-mode "Buildbot step"
  "Buildbot view for a step")

(defvar-local buildbot-step-revision-info nil)
(defvar-local buildbot-step-build nil)
(defvar-local buildbot-step-step nil)
(defun buildbot-step-buffer-name (stepid)
  (concat "*buildbot step " (number-to-string stepid) "*"))

(defun buildbot-step-load (revision-info build step)
  (let ((buffer-name (buildbot-step-buffer-name (alist-get 'stepid step))))
    (with-current-buffer (get-buffer-create buffer-name)
      (buildbot-step-mode)
      (setq buildbot-step-revision-info revision-info
            buildbot-step-build build
            buildbot-step-step step)
      (buildbot-step-update))
    (switch-to-buffer buffer-name)))

(defun buildbot-step-update ()
  (unless (derived-mode-p 'buildbot-step-mode)
    (error "Not in buildbot step mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((logs (buildbot-get-logs-by-stepid
                 (alist-get 'stepid buildbot-step-step))))
      (insert (buildbot-step-format
               buildbot-step-revision-info
               buildbot-step-build
               buildbot-step-step
               logs))
      (goto-char (point-min)))))

(defun buildbot-step-reload ()
  (interactive)
  (buildbot-step-update))
(define-key buildbot-step-mode-map "g" 'buildbot-step-reload)

(defun buildbot-step-format (revision-info build step logs)
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build build)
   "\n"
   (buildbot-view-format-step step)
   (string-join
    (mapcar 'buildbot-view-format-log logs)
    "\n")))

(provide 'buildbot-step)
