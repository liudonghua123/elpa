;;; rt-liberation-report.el --- Emacs interface to RT

;; Copyright (C) 2015  Free Software Foundation
;;
;; Authors: Yoni Rabkin <yrk@gnu.org>
;;
;; This file is a part of rt-liberation.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; History:
;;
;; I wrote rt-report.py sometime in 2013 because people asked for some
;; information as to how many tickets were being resolved, and by
;; whom. When in came time up update rt-report.py I came to my senses
;; and decided to re-write it in Emacs Lisp as a part of
;; rt-liberation.


;;; Code:

(require 'rt-liberation)
(require 'rt-liberation-rest)


(defvar rt-liber-report-csv-header
  '("date" "tickets resolved")
  "Headers for comma separated value output.")

(defun rt-liber-report-get-interval (rt-queue start-date end-date)
  "Return tickets resolved between START-DATE and END-DATE.

The tickets must have their current status be Resolved in order
to be returned by this function. If no tickets match the query,
return `nil'."
  (when (or (not (stringp rt-queue))
	    (not (stringp start-date))
	    (not (stringp end-date)))
    (error "bad argument/s"))
  (rt-liber-rest-run-show-base-query
   (rt-liber-rest-run-ls-query
    (rt-liber-compile-query
     (and (queue    rt-queue)
	  (resolved end-date start-date)
	  (status   "resolved"))))))

(defun rt-liber-report-scan-ticket (ticket-alist)
  "Convert TICKET-ALIST to set format."
  (let ((date-resolved (cdr (assoc "Resolved" ticket-alist)))
	(owner         (cdr (assoc "Owner" ticket-alist))))
    `(,(float-time (date-to-time date-resolved)) . ,owner)))

(defun rt-liber-report-scan-interval (interval)
  "Convert the list of tickets into an ordered format."
  (when (not interval)
    (error "no tickets in interval"))
  (let ((l (copy-tree interval))
	(r nil))
    (while l
      (setq r (append r `(,(rt-liber-report-scan-ticket (car l)))))
      (setq l (cdr l)))
    ;; sort the list when it is still in seconds format
    (setq r (sort r
		  #'(lambda (a b)
		      (< (car a) (car b)))))
    ;; change the sorted list by day-date format, so that we can
    ;; pigeon-hole count by day later on
    (dolist (e r)
      (setcar e (format-time-string "%Y-%m-%d" (car e))))
    r))

(defun rt-liber-report-count (f l)
  "Apply function F to list L to produce a count."
  (let (out)
    (while l
      (let* ((head (car l))
	     (old-value (cdr (assoc (funcall f head) out))))
	(if old-value
	    (setcdr (assoc (funcall f head) out) (+ old-value 1))
	  (setq out (append out `((,(funcall f head) . 1))))))
      (setq l (cdr l)))
    out))

(defun rt-liber-report-count-total (l)
  (let ((c 0))
    (while l
      (setq c (+ c (cdr (car l))))
      (setq l (cdr l)))
    c))

(defun rt-liber-report-count-by-date (l)
  "Count resolved tickets by date."
  (rt-liber-report-count #'car l))

(defun rt-liber-report-count-by-owner (l)
  "Count resolved tickets by owner."
  (rt-liber-report-count #'cdr l))

(defun rt-liber-report-print-csv (header l)
  "Output list L in a CSV format, starting with HEADER."
  (let (out)
    (with-temp-buffer
      (insert (format "\n%s\n" header))
      (dolist (entry l)
	(insert
	 (format "%s, %s\n" (car entry) (cdr entry))))
      (setq out (buffer-string)))
    out))

(defun rt-liber-report (rt-queue start-date end-date)
  "Print tickets resolved between START-DATE and END-DATE."
  (let ((tickets (rt-liber-report-scan-interval
		  (rt-liber-report-get-interval
		   rt-queue start-date end-date)))
	by-date by-owner
	by-date-out
	by-owner-out
	total)
    (when (not tickets)
      (error (concat "no tickets in interval between "
		     start-date " and " end-date)))
    ;; collate
    (setq by-date  (rt-liber-report-count-by-date tickets)
	  by-owner (rt-liber-report-count-by-owner tickets))
    ;; rank owners by resolved tickets
    (setq by-owner
	  (sort
	   by-owner
	   #'(lambda (a b)
	       (> (cdr a) (cdr b)))))
    ;; count total
    (setq total (rt-liber-report-count-total by-owner))
    ;; print
    (insert (rt-liber-report-print-csv "date, resolved" by-date))
    (insert (rt-liber-report-print-csv "owner, resolved" by-owner))
    (insert (format "\ntotal tickets resolved: %d\n" total))))


(provide 'rt-liberation-report)


;;; rt-liberation-report.el ends here.
