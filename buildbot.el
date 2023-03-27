;;; buildbot.el --- A Buildbot client for emacs -*- lexical-binding: t; -*-

;; Author: Yuchen Pei <id@ypei.org>
;; Maintainer: Yuchen Pei <id@ypei.org>
;; Created: 2023
;; Version: 1.0.0
;; Keywords: buildbot
;; Package-Requires: ((emacs "28"))
;; Package-Type: multi
;; Homepage: https://g.ypei.me/buildbot.el.git

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
;; You should have received a copy of the GNU Affero General Public License
;; along with buildbot.el.  If not, see <https://www.gnu.org/licenses/>.
(require 'buildbot-client)
(require 'buildbot-view)

(provide 'buildbot)
