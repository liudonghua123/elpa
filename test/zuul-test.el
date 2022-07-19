;;; zuul-test.el --- Tests for zuul.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Niklas Eklund

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

;; Tests for `zuul'.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'zuul)

;;;; Tests

(ert-deftest zuul-test-tenant-config ()
  (let ((zuul-tenant-configs '((:name "foo")))
        (zuul-tenant "foo")
        (expected '(:name "foo")))
    (should (equal expected (zuul--tenant-config)))
    (let ((zuul-tenant "bar"))
      (should (not (zuul--tenant-config))))))

(ert-deftest zuul-test-status ()
  ;; Build
  (let ((build-with-result (zuul--build-create :data '((result . "SUCCESS"))))
        (build-running (zuul--build-create :data '((start_time . t)))))
    (should (string= "SUCCESS" (zuul--status build-with-result)))
    (should (string= "RUNNING" (zuul--status build-running)))
    (should (string= "QUEUED" (zuul--status (zuul--build-create)))))

  ;; Buildset
  (let ((buildset-with-result (zuul--buildset-create :data '((result . "SUCCESS"))))
        (buildset-running (zuul--buildset-create :data '((first_build_start_time . t)))))
    (should (string= "SUCCESS" (zuul--status buildset-with-result)))
    (should (string= "RUNNING" (zuul--status buildset-running)))
    (should (string= "QUEUED" (zuul--status (zuul--buildset-create))))))

(provide 'zuul-test)

;;; zuul-test.el ends here
