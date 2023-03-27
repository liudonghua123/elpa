;; -*- lexical-binding: t; -*-
(require 'buildbot-client)
(defvar buildbot-builders (buildbot-get-all-builders))
(require 'buildbot-view)

(provide 'buildbot)
