;;; auto-overlay-common.el --- general overlay functions


;; Copyright (C) 2005-2015  Free Software Foundation, Inc

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Maintainer: Toby Cubitt <toby-predictive@dr-qubit.org>
;; URL: http://www.dr-qubit.org/emacs.php
;; Repository: http://www.dr-qubit.org/git/predictive.git

;; This file is part of the Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(provide 'auto-overlay-common)


(defun auto-o--plist-delete (plist &rest keys)
  ;; Destructively delete KEYS from PLIST
  (while (memq (car plist) keys)
    (setq plist (cddr plist)))
  (let ((el (cdr plist)))
    (while el
      (when (memq (cadr el) keys)
	(setcdr el (cdddr el)))
      (setq el (cddr el))))
  plist)


(defun auto-overlay-< (a b)
  "Return non-nil iff overlay A comes before overlay B in buffer."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (or (< (overlay-start a) (overlay-start b))
	   (and (= (overlay-start a) (overlay-start b))
		(> (overlay-end a) (overlay-end b))))))


;;;###autoload
(cl-defun auto-overlays-at-point (&optional point
				  &rest prop-tests
				  &key inactive all-overlays &allow-other-keys)
  "Return overlays overlapping POINT, defaulting to the point.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments specify property tests, each of which
should be a list with one of the following forms:

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned."

  (when (null point) (setq point (point)))
  (auto-overlay-trigger-update point)

  (let (overlay-list)
    ;; get overlays overlapping POINT and zero-length overlays at POINT
    (setq overlay-list
	  (apply #'auto-overlays-in point point prop-tests))
    ;; get overlays that end at POINT
    (dolist (o (apply #'auto-overlays-in (1- point) point prop-tests))
      (when (and (< (overlay-start o) point)
		 (= (overlay-end o) point))
	(push o overlay-list)))
    ;; get overlays that start at POINT
    (dolist (o (apply #'auto-overlays-in point (1+ point) prop-tests))
      (when (and (> (overlay-end o) point)
		 (= (overlay-start o) point))
	(push o overlay-list)))
    (sort overlay-list #'auto-overlay-<)))



;;;###autoload
(cl-defun auto-overlays-in (start end &rest prop-tests
				  &key within inactive all-overlays &allow-other-keys)
;; FIXME: get rid of INACTIVE argument?
  "Return auto overlays overlapping region between START and END.

If keyword argument :within is non-nil, only overlays entirely
within START and END are returned.

If keyword argument :inactive is non-nil, both active and
inactive overlays are returned (usually inactive ones are
ignored).

If keyword argument :all-overlays is non-nil, all overlays are
returned, not just auto-overlays.

Any remaining arguments specify property tests, each of which
should be a list with one of the following forms:

  (FUNCTION PROPERTY)

  (FUNCTION PROPERTY VALUE)

  (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))

where PROPERTY indicates an overlay property name (a symbol), and
VALUE indicates an arbitrary value or lisp expression.

For each overlay between START and END, first the values
corresponding to the property names are retrieved from the
overlay, then FUNCTION is called with the properties values
followed by the other values as its arguments. The test is
satisfied if the result is non-nil, otherwise it fails. Tests are
evaluated in order, but only up to the first failure. Only
overlays that satisfy all property tests are returned."

  ;; remove any keyword arguments from PROP-TESTS
  (setq prop-tests
	(auto-o--plist-delete prop-tests :within :inactive :all-overlays))
  ;; exclude inactive overlays unless told not to
  (unless inactive (push '(null inactive) prop-tests))
  ;; exclude non-auto-overlays unless told not to
  (unless all-overlays (push '(identity auto-overlay) prop-tests))

  ;; FIXME: Is updating just START and END enough to trigger all updates?
  (auto-overlay-trigger-update start)
  (unless (= start end) (auto-overlay-trigger-update end))

  (let (overlay-list function prop-list value-list result)
    ;; check properties of each overlay in region
    (dolist (o (overlays-in start end))
      ;; check overlay is entirely within region
      (if (and within
	       (or (< (overlay-start o) start) (> (overlay-end o) end)))
	  (setq result nil)

	;; if it is, or we don't care
	(setq result t)
	(catch 'failed
	  ;; check if properties match
	  (dolist (test prop-tests)
	    ;; (Note: the whole thing would be neater with something like
	    ;; (apply 'and (map ...)) but 'and is a special form, not a
	    ;; function, so can't be applied)
	    (setq function (nth 0 test))
	    (unless (listp (setq prop-list (nth 1 test)))
	      (setq prop-list (list prop-list)))
	    (setq value-list nil)
	    (unless (or (< (length test) 3)
			(and (setq value-list (nth 2 test))  ; nil isn't list
			     (listp value-list)))
	      (setq value-list (list value-list)))

	    ;; apply the test
	    (setq result
		  (and result
		       (apply function
			      (append (mapcar (lambda (p) (overlay-get o p))
					      prop-list)
				      value-list))))
	    (when (null result) (throw 'failed nil)))))

      ;; add overlay to result list if its properties matched
      (when result (push o overlay-list)))
    ;; return result list
    (nreverse overlay-list)))



;;;###autoload
(cl-defun auto-overlay-highest-priority-at-point (&optional point
					          &rest prop-tests
						  &key inactive all-overlays
						  &allow-other-keys)
  "Return highest priority overlay at POINT, defaulting to the point.

If two overlays have the same priority, the innermost one takes
precedence (i.e. the one that begins later, or if they begin at
the same point the one that ends earlier).

The remaining arguments are as for `auto-overlays-at' (which see)."

  (unless point (setq point (point)))

  ;; get all overlays at point with a non-nil SYMBOL property
  (let* ((overlay-list (apply #'auto-overlays-at-point point prop-tests))
	 (overlay (pop overlay-list))
	 p p1)

    ;; find the highest priority, innermost overlay
    (dolist (o1 overlay-list)
      (setq p (overlay-get overlay 'priority))
      (setq p1 (overlay-get o1 'priority))
      (when (or (and (null p) p1)
		(and p p1 (> p1 p))
		(and (equal p1 p)
		     (or (> (overlay-start o1) (overlay-start overlay))
			 (and (= (overlay-start o1) (overlay-start overlay))
			      (< (overlay-end o1) (overlay-end o1))))))
	(setq overlay o1)))

    ;; return the overlay
    overlay))



;;;###autoload
(defun auto-overlay-local-binding (symbol &optional point only-overlay)
  "Return \"overlay local \" binding of SYMBOL at POINT,
or the current local binding if there is no overlay binding. If
there is no overlay binding and SYMBOL is not bound, return
nil. POINT defaults to the point.

If ONLY-OVERLAY is non-nil, only overlay bindings are
returned. If none exists at POINT, nil is returned

An \"overlay local\" binding is created by giving an overlay a
non-nil value for a property named SYMBOL. If more than one
overlay at POINT has a non-nil SYMBOL property, the value from
the highest priority overlay is returned.

See `auto-overlay-highest-priority-at-point' for a definition of
\"highest priority\"."

  (let ((overlay (auto-overlay-highest-priority-at-point
		  point `(identity ,symbol))))
    (if overlay
	(overlay-get overlay symbol)
      (and (not only-overlay) (boundp symbol) (symbol-value symbol)))))

;; auto-overlay-common.el ends here
