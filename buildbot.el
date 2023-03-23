;; -*- lexical-binding: t; -*-
(defvar buildbot-client-buffer-name "*buildbot api*")
(defvar buildbot-host nil)

(require 'buildbot-client)
(defvar buildbot-builders (buildbot-get-all-builders))
(require 'buildbot-revision)
(require 'buildbot-build)

(provide 'buildbot)
