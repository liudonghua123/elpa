;;; gnus-mock-tests.el --- Interactive tests for Gnus Mock  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides ERT-based interactive tests for Gnus Mock.

;; "Normal" tests for Gnus are part of the Emacs codebase, and are run
;; in batch mode via the "make" script.  They test Gnus' functions and
;; data structures, but it would be impractical/impossible for them to
;; start up an actual Gnus instance and test its behavior.  That's
;; what this library is for: it contains a test suite that is
;; initiated interactively from a running Gnus Mock instance.  It's
;; meant to ensure that actions like group sorting, thread toggling,
;; and article moving/copying/deleting not only don't raise an error,
;; but result in a Gnus state that _looks_ like it ought to.

;; The tests can be run in two modes: "top-speed", and
;; "stop-and-look".  The former programmatically checks that the
;; results of various operations have produced the desired results;
;; the latter additionally inserts `sit-for' calls between tests, so
;; that the user can visually confirm that nothing weird has happened.

;; The entry point for the "top-speed" mode is `gnus-mock-run-tests';
;; for the "stop-and-look" mode, it's `gnus-mock-run-tests-halt'.

;; These tests are tied directly to the dummy data provided in Gnus
;; mock.  If that data changes at all, these tests will almost
;; certainly have to be adjusted accordingly.

;; The code tries to use `call-interactively' where possible, but it's
;; not always possible (when passing specific interactive arguments.
;; It also attempts to leave Gnus the way it was when it started, but
;; that's not always possible either.

;;; Code:

(require 'ert)
(require 'gnus)

(autoload 'gnus-topic-mode "gnus-topic")
(defvar gnus-topic-mode nil)

(defcustom gnus-mock-halt-seconds 2
  "In `gnus-mock-run-tests-halt', halt for this many seconds."
  :group 'gnus-mock
  :type '(integer
	  :tag "Number of seconds to halt for"
	  :validate
	  (lambda (widg)
	    (let ((val (widget-value widg)))
	      (unless (> val 0)
		(widget-put widg :error "Must be a positive integer")
		widg)))))

(defvar gnus-mock-halt nil
  "When non-nil, pause at key points during the testing process.
This gives the user a chance to confirm that everything looks the
way it ought to.

This variable shouldn't be set directly, it is let-bound inside
`gnus-mock-run-tests-halt'.")

(defsubst gnus-mock-maybe-halt (&optional message)
  (when gnus-mock-halt
    (with-temp-message message
      (sit-for gnus-mock-halt-seconds))))

(defmacro with-topic-mode-on (&rest body)
  `(let ((was-topic (if gnus-topic-mode 1 -1)))
     (gnus-topic-mode 1)
     (unwind-protect
	 (progn ,@body)
       (let ((current-prefix-arg was-topic))
	 ;; `gnus-topic-mode' only redisplays the group buffer if it
	 ;; was called interactively, and it's very confusing not to
	 ;; have the buffer redisplayed.
	 (call-interactively #'gnus-topic-mode)))))

(defmacro with-topic-mode-off (&rest body)
  `(let ((was-topic (if gnus-topic-mode 1 -1)))
     (gnus-topic-mode -1)
     (unwind-protect
	 (progn ,@body)
       (let ((current-prefix-arg was-topic))
	 (call-interactively #'gnus-topic-mode)))))

(defun gnus-mock-run-tests ()
  (interactive)
  (unless (gnus-alive-p)
    (user-error "Start Gnus before running tests"))
  (call-interactively #'ert))

(defun gnus-mock-run-tests-halt ()
  (interactive)
  (let ((gnus-mock-halt t))
    (call-interactively #'gnus-mock-run-tests)))

(ert-deftest gnus-mock-test-sanity ()
  "Sanity test."
  (let ((number-of-groups (hash-table-count gnus-newsrc-hashtb)))
    (gnus-mock-maybe-halt "Hi there, you're testing.")
    (should (= number-of-groups 10))
    (with-current-buffer gnus-group-buffer
      (should (= (count-lines (point-min) (point-max)) 5)))))

(ert-deftest gnus-mock-test-sort-flat-alpha ()
  "Test sorting group names alphabetically."
  (with-current-buffer gnus-group-buffer
    (with-topic-mode-off
     (call-interactively #'gnus-group-sort-groups-by-alphabet)
     (gnus-mock-maybe-halt "Groups sorted by name")
     (goto-char (point-min))
     (should (string-equal (gnus-group-group-name) "Welcome"))
     (let ((current-prefix-arg '(4)))
       (call-interactively #'gnus-group-sort-groups-by-alphabet))
     (goto-char (point-min))
     (gnus-mock-maybe-halt "Groups sorted by reverse name")
     (should (string-equal (gnus-group-group-name) "nnimap+Mocky:Приветмир")))))

(ert-deftest gnus-mock-test-sort-topics-alpha ()
  (with-current-buffer gnus-group-buffer
    (with-topic-mode-on
     (gnus-topic-jump-to-topic "misc")
     (gnus-topic-sort-groups-by-alphabet)
     (forward-line)
     (gnus-mock-maybe-halt "Topic groups sorted by name")
     (should (string-equal (gnus-group-group-name) "Welcome"))
     (gnus-topic-jump-to-topic "misc")
     (let ((current-prefix-arg '(4)))
       (call-interactively #'gnus-topic-sort-groups-by-alphabet))
     (forward-line)
     (gnus-mock-maybe-halt "Topic groups sorted by reverse name")
     (should (string-equal (gnus-group-group-name) "nnimap+Mocky:Приветмир")))))

(ert-deftest gnus-mock-make-multibyte-group ()
  "Test creation of a nnml group with non-ascii name."
  (with-current-buffer gnus-group-buffer
    (unwind-protect
	(progn
	  (gnus-group-make-group "测试" "nnml:")
	  (gnus-mock-maybe-halt)
	  (should (gnus-group-goto-group "nnml:测试"))
	  (gnus-mock-maybe-halt)
	  (should (gnus-group-entry "nnml:测试"))
	  (should (gnus-active "nnml:测试"))
	  (gnus-group-save-newsrc)
	  (with-temp-buffer
	    (insert-file-contents
	     (concat gnus-current-startup-file ".eld"))
	    ;; Group names should now be saved with proper encoding.
	    (should (re-search-forward "nnml:测试"))))
      (gnus-group-delete-group "nnml:测试" t t))))

(ert-deftest gnus-mock-copy-article-to-multibyte-group ()
  "Test copying an article to a group with non-ascii name."
  (with-current-buffer gnus-group-buffer
    (with-topic-mode-off
     (unwind-protect
	 (progn
	   (gnus-group-make-group "หลอดทดลอง" "nnml:")
	   (gnus-mock-maybe-halt "Made new group")
	   (gnus-group-jump-to-group "mails" nil)
	   (call-interactively #'gnus-group-select-group)
	   (gnus-mock-maybe-halt)
	   (gnus-summary-move-article 1 "nnml:หลอดทดลอง")
	   ;; Do we need to `should' anything?  It ought to be enough
	   ;; that no error was raised.
	   (gnus-mock-maybe-halt "Moved article")
	   (call-interactively #'gnus-summary-exit)
	   (gnus-group-jump-to-group "nnml:หลอดทดลอง")
	   (call-interactively #'gnus-group-select-group)
	   (gnus-mock-maybe-halt)
	   (should (= 1 (length gnus-newsgroup-data)))
	   (call-interactively #'gnus-summary-exit))
       (gnus-group-delete-group "nnml:หลอดทดลอง" t t)))))

(provide 'gnus-mock-tests)
;;; gnus-mock-tests.el ends here
