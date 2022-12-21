;;; heaps.el --- Some kind of heap data structure of Dismal  -*- lexical-binding: t; -*-

;; Copyright (C) 1992-2021  Free Software Foundation, Inc.

;; Author: David Fox, fox@cs.nyu.edu
;; Created-On: Mon Jan  6 14:19:10 1992

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: Use GNU ELPA's `heap' package instead!

;;; Code:

;;;; IV.	Heaps

;; PRIORITY QUEUE - A heap, implemented as a 4-tuple:
;;      [compare-function
;;       vector-size
;;       element-count
;;       element-vector]
;;
;; Entry points:
;;      (heaps-create compare-function)  - Create an empty heap
;;      (heaps-insert heap element)      - Insert an element
;;      (heaps-deletemin heap)           - Delete and return smallest
;;      (heaps-empty-p heap)             - Empty heap predicate
;;      (heaps-size heap)                - Number of elements in the heap

(cl-defstruct (heaps
               (:conc-name heaps--)
               (:constructor nil)
               (:constructor heaps-create (compare-function)))
  compare-function
  (space 1)
  (last 0)
  (elems (make-vector 1 nil)))
               
(defsubst heaps--compare (h a b)
  "Use HEAP's compare function to compare elements A and B."
  (funcall (heaps--compare-function h) a b))

(defalias 'heaps-size #'heaps--last
  "Number of elements in the heap.")

(defsubst heaps--aref (h n)
  "Return the HEAP's Nth element."
  (declare (gv-expander (lambda (do)
                          (macroexp-let2 nil v `(heaps--elems ,h)
                            (macroexp-let2 nil n n
                              (gv-get `(aref ,v ,n) do))))))
  (aref (heaps--elems h) n))

(defsubst heaps-empty-p (h)
  "Return non-nil iff HEAP is empty."
  (= (heaps--last h) 0))

(defsubst heaps--swap (h i j)
  "Swap HEAP's I'th and J'th elements."
  (let ((elem1 (heaps--aref h i))
        (elem2 (heaps--aref h j)))
    (setf (heaps--aref h i) elem2)
    (setf (heaps--aref h j) elem1)))

(defun heaps--bubble-up (heap index)
  "Helping function for `heaps-insert'."
  (let* ((half (/ (1- index) 2))
         (elem (heaps--aref heap index))
         (parent (heaps--aref heap half))
         (comp (heaps--compare heap parent elem)))
    (if (<= comp 0)
        ()
      (setf (heaps--aref heap index) parent)
      (setf (heaps--aref heap half) elem)
      (if (> index 0)
          (heaps--bubble-up heap half)))))


(defsubst heaps-insert (heap element)
  "Insert ELEMENT into HEAP."
  ;; if there is no space, grow the heap doubling it
  (if (= (heaps--space heap) (heaps--last heap))
      (progn
        (cl-callf (lambda (x) (vconcat x (make-vector (heaps--space heap) nil)))
            (heaps--elems heap))
        (setf (heaps--space heap) (+ (heaps--space heap)
                                     (heaps--space heap)))))
  ;; Check to see if element is in heap
  ;; there may be a smarter way, but this will work
  (if (heaps--member element heap)
      nil ;; duplicate caught
    ;; Else
    ;; Put the new element in the next free position in the heap vector
    (setf (heaps--aref heap (heaps--last heap)) element)
    ;; Increment the element count
    (let ((last (heaps--last heap)))
      (if (> last 0)
          (heaps--bubble-up heap last))
      (setf (heaps--last heap) (1+ last)))))

(defun heaps-deletemin (heap)
  "Delete and return the minimum element from the HEAP."
  (if (heaps-empty-p heap)
      nil
    (setf (heaps--last heap) (1- (heaps--last heap)))
    (let* ((minelem (heaps--aref heap 0))
           (lastelem (heaps--aref heap (heaps--last heap))))
      (setf (heaps--aref heap 0) lastelem)
      (heaps--bubble-down heap 0)
      minelem)))

(defsubst heaps--index-of-min (heap i j)
  "Given a HEAP and two indices I and J, return the index that points
to the lesser of the corresponding elements."
  (if (> 0 (heaps--compare heap (heaps--aref heap i) (heaps--aref heap j))) i j))
      
(defun heaps--bubble-down (heap index)
  "Helper function for heaps-deletemin."
  (let* ((leftindex (+ index index 1))
         (rightindex (+ leftindex 1))
         (minchild))
    (if (>= leftindex (heaps--last heap)) ; if no left child
        ()
      (if (>= rightindex (heaps--last heap)) ; if no right child
          (setq minchild leftindex)
        (setq minchild (heaps--index-of-min heap leftindex rightindex)))
      (if (not (= (heaps--index-of-min heap index minchild) minchild))
          ()
        (heaps--swap heap index minchild)
        (heaps--bubble-down heap minchild)))))

(defun heaps--member (element heap)
  "Return t if element is in heap."
  ;; assume that heap is a heap  
  ;; brute force (should be faster with a binary search, as in  a HEAP!
  (if (heaps-empty-p heap) nil
  (let ((compare-fun (heaps--compare-function heap))
        (i 0) (result nil)
        (last (heaps--last heap)))
     (while (and (< i last) (not result)) 
        (if (= 0 (funcall compare-fun element (heaps--aref heap i)))
            (setq result t))
        (setq i (+ 1 i)))
      result)))

;; (heaps--member '(2 . 2) a)

;; Some test code:
;;
;;  (setq dismal-invalid-heapA (heaps-create #'dismal-address-compare))
;;  (setq dismal-invalid-heap dismal-invalid-heapA)
;;  
;;  (heaps-empty-p dismal-invalid-heap)
;;  (heaps-insert dismal-invalid-heap (cons 2 0))
;;  (heaps-insert dismal-invalid-heap (cons 3 0))
;;  (heaps-insert dismal-invalid-heap (cons 4 0))
;;  (heaps-insert dismal-invalid-heap (cons 4 0))
;;  (heaps-insert dismal-invalid-heap (cons 3 0))
;;  (heaps-insert dismal-invalid-heap (cons 2 0))
;;  
;;  (setq addr (heaps-deletemin dismal-invalid-heap))

(provide 'heaps)
;;; heaps.el ends here
