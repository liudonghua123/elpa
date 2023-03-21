;; -*- lexical-binding: t; -*-
(defvar buildbot-builders (buildbot-get-all-builders))
(defvar buildbot-client-buffer-name "*buildbot api*")
(defvar buildbot-host nil)

(require 'buildbot-revision)
(require 'buildbot-build)
(require 'buildbot-client)

(provide 'buildbot)
