;;; el-search-hi-lock.el --- hi-lock with el-search patterns    -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2018_01_14
;; Keywords: lisp


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements the counterpart of hi-lock.el for el-search
;; patterns: Permanent highlighting of matches of specified patterns
;; that is automatically updated when the buffer is edited.  Unlike
;; hi-lock, and in contrast to what the name "el-search-hi-lock"
;; suggests, we can't use font-lock for this purpose.  Instead, we use
;; a timer to highlight the visible portions of the buffer.
;;
;; The entry points are `el-search-hi-lock-mode' to turn highlighting
;; on and off, `el-search-hi-lock-add-pattern' to add patterns to be
;; highlighted with specified faces (automatically turns on
;; `el-search-hi-lock-mode'), and `el-search-hi-lock-remove-pattern'
;; (removes patterns from the list of patterns to be highlighted).
;;
;; `el-search-hi-lock-add-pattern' can also be used in file and
;; directory local variable specifications (with `eval').
;;
;; This is a bit slower than hi-lock.  Don't use it with too costly
;; patterns to avoid Emacs becoming sluggish.

;;; Code:

(require 'el-search)
(eval-when-compile (require 'subr-x))
(require 'hi-lock) ;faces

(defgroup el-search-hi-lock nil
  "Doc..."
  :group 'el-search)

;; These faces definitions are stolen from Drew's "highlight.el"
(defface el-search-hi-lock-decent-1
  '((((background dark)) (:background "#333333")) ;gray
    (t (:background "#BBEEBB"))) ;a light green
  "Doc...")
(defface el-search-hi-lock-decent-3
  '((((background dark)) (:background "#04602BC00000")) ; a very dark green
    (t (:background "#FCFCE1E1FFFF"))) ; a light magenta
  "Doc...")
(defface el-search-hi-lock-decent-2
  '((((background dark)) (:background "#316B22970000")) ; a very dark brown
    (t (:background "#E1E1EAEAFFFF"))) ; a light blue
  "Doc...")
(defface el-search-hi-lock-decent-4
  '((((background dark)) (:background "#00234F")) ; a dark blue
    (t (:background "#E3FF9A"))) ; a light yellow
  "Doc...")

(defvar el-search-hi-lock-warning-time .1
  "Doc...")


(defvar-local el-search-hi-lock-current-patterns '()
  "Elements have the form (PATTERN MATCHER HM FACE).")

(defvar-local el-search-hi-lock-overlays '())
(defvar el-search-hi-lock-extra-window-heights 1.)

(defmacro el-search-hi-lock--while-no-input (&rest body)
  "Like `while-no-input' but without preceding `input-pending-p' test."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
	   ,@body)))))

(defvar el-search-hi-lock-window-in-progress nil)

(defun el-search-hi-lock-window (&optional window)
  ;; Return done when successfully hi-locked without user interruption,
  ;; error when catched an error
  (cl-callf or window (selected-window))
  (let ((el-search-hi-lock-window-in-progress t))
    (with-current-buffer (window-buffer window)
      (condition-case nil
          (let ((here (window-point window))
                (start (window-start window))
                (end  (window-end window t))
                (add-overlay (lambda (beg end face &optional priority)
                               (let ((ov (make-overlay beg end)))
                                 (push ov el-search-hi-lock-overlays)
                                 (overlay-put ov 'face face)
                                 (overlay-put ov 'priority (or priority 100)))))
                (delete-old-overlays
                 (let ((overlays (copy-sequence el-search-hi-lock-overlays)))
                   (lambda ()
                     (mapc #'delete-overlay overlays)
                     (cl-callf cl-set-difference el-search-hi-lock-overlays overlays)))))
            (when (numberp el-search-hi-lock-extra-window-heights)
              (let ((window-lines (count-lines start end)))
                (setq start (save-excursion
                              (goto-char start)
                              (line-beginning-position
                               (round (* -1 el-search-hi-lock-extra-window-heights window-lines)))))
                (setq  end  (save-excursion
                              (goto-char end)
                              (line-beginning-position
                               (round (* el-search-hi-lock-extra-window-heights window-lines)))))))
            (el-search-hi-lock--while-no-input
              (save-excursion
                (goto-char start)
                (let (string-or-comment-begin)
                  (while (and (not (bobp))
                              (setq string-or-comment-begin (nth 8 (syntax-ppss))))
                    (goto-char string-or-comment-begin)
                    (when (not (bobp))
                      (backward-char))))
                (when-let ((pos (and (not (bobp))
                                     (ignore-errors (scan-sexps (point) -1)))))
                  (goto-char pos))
                (while (and (not (bobp))
                            (condition-case nil (progn (backward-up-list 1) t)
                              (scan-error nil)))
                  (mapc
                   (pcase-lambda (`(_pattern ,matcher ,_hm ,face ,priority))
                     (when (el-search--looking-at-1 matcher)
                       (funcall add-overlay (point) (el-search--end-of-sexp) face priority)))
                   el-search-hi-lock-current-patterns)))
              (let ((bound end))
                (save-excursion
                  (goto-char end)
                  (let ((done nil))
                    (while (and (not done)
                                (not (bobp))
                                (condition-case nil (progn (backward-up-list 1) t)
                                  (scan-error nil)))
                      (if (< start (point)) (setq bound (el-search--end-of-sexp))
                        (setq done t)))))
                (save-excursion
                  (mapc
                   (pcase-lambda (`(,pattern ,matcher ,hm ,face ,priority . (,pred)))
                     (let (this-match-beg this-match-end (done nil))
                       (let* ((start-time (and el-search-hi-lock-warning-time (current-time)))
                              (maybe-warn-about-slow-pattern
                               (lambda ()
                                 (when (and start-time
                                            (< el-search-hi-lock-warning-time
                                               (time-to-seconds
                                                (time-subtract (current-time) start-time))))
                                   (setq start-time nil)
                                   (message "\
el-search-hi-lock warning: slow fontification with pattern %S"
                                            pattern)))))
                         (save-excursion
                           (goto-char start)
                           (while (not done)
                             (setq this-match-beg
                                   (el-search--search-pattern-1 matcher t bound hm))
                             (funcall maybe-warn-about-slow-pattern)
                             (if (or (not this-match-beg)
                                     (< end (point)))
                                 (setq done t)
                               (goto-char this-match-beg)
                               (setq this-match-end (el-search--end-of-sexp))
                               (unless (or (< this-match-end start)
                                           (and pred (let ((there (point)))
                                                       (save-excursion
                                                         (goto-char here)
                                                         (not (funcall pred there))))))
                                 (funcall add-overlay this-match-beg this-match-end face priority))
                               (goto-char this-match-end)
                               (when (>= (point) end)
                                 (setq done t))))))))
                   el-search-hi-lock-current-patterns)))
              (let ((window-scroll-functions
                     (remq #'el-search-hi-lock-trigger-update window-scroll-functions)))
                ;; Without this, highlight updating may get indefinitely triggered
                ;; from w-scroll-functions
                (let ((throw-on-input nil))
                  (funcall delete-old-overlays)
                  (unless el-search-hi-lock-window-in-progress ;this may infrec
                    (redisplay))
                  'done))))
        ((scan-error end-of-buffer end-of-file invalid-read-syntax)
         ;; Main case: incomplete sexps while typing
         'error)))))

(defvar-local el-search--hi-lock-timer nil)

(defvar el-search-hi-lock-mode)

(defvar el-search-hi-lock-idle-time 0)
(defvar el-search-hi-lock-idle-after-change-time 1.)

(defun el-search-hi-lock-timer-function (window buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (timerp el-search--hi-lock-timer)
        (cancel-timer el-search--hi-lock-timer)
        (setq el-search--hi-lock-timer nil)))
    (when (window-live-p window)
      (with-current-buffer buffer
        (when el-search-hi-lock-mode
          (unless (member (el-search-hi-lock-window window) (list 'error 'done))
            ;; User interrupt.  Retry.
            (el-search-hi-lock-trigger-update window 'retry)))))))

(defun el-search-hi-lock-trigger-update (&rest args)
  (cl-flet ((do-it (window &optional delay)
                   (when (window-live-p window)
                     (with-current-buffer (window-buffer window)
                       (if (timerp el-search--hi-lock-timer)
                           (progn
                             (when delay
                               (timer-set-time el-search--hi-lock-timer
                                               (time-add (current-time) delay)))
                             (timer-activate el-search--hi-lock-timer))
                         (setq-local el-search--hi-lock-timer
                                     (run-with-idle-timer
                                      (or delay el-search-hi-lock-idle-time) nil
                                      #'el-search-hi-lock-timer-function
                                      window (window-buffer window))))))))
    (pcase args
      (`(,(and (pred framep) frame)) (mapc #'el-search-hi-lock-trigger-update (window-list frame)))
      (`(,(and (pred windowp) window) retry) (do-it window .15))
      (`(,(and (pred windowp) window) . ,_)  (do-it window))
      (_                                     (do-it (selected-window)
                                                    el-search-hi-lock-idle-after-change-time)))))

(defun el-search-hi-lock-update-buffer-windows (&optional buffer)
  (mapc #'el-search-hi-lock-window (get-buffer-window-list (or buffer (current-buffer)) nil t)))

;;;###autoload
(define-minor-mode el-search-hi-lock-mode
  "Doc..."
  nil (:eval (if el-search-hi-lock-current-patterns " ElHi" "")) nil
  (if el-search-hi-lock-mode
      (progn
        (el-search-hi-lock-update-buffer-windows)
        (add-hook 'after-change-functions           #'el-search-hi-lock-trigger-update t t)
        (add-hook 'window-scroll-functions          #'el-search-hi-lock-trigger-update t t)
        (add-hook 'window-size-change-functions     #'el-search-hi-lock-trigger-update t t)
        (add-hook 'window-configuration-change-hook #'el-search-hi-lock-trigger-update t t))
    (remove-hook 'after-change-functions            #'el-search-hi-lock-trigger-update t)
    (remove-hook 'window-scroll-functions           #'el-search-hi-lock-trigger-update t)
    (remove-hook 'window-size-change-functions      #'el-search-hi-lock-trigger-update t)
    (remove-hook 'window-configuration-change-hook  #'el-search-hi-lock-trigger-update t)
    (mapc #'delete-overlay el-search-hi-lock-overlays)
    (setq el-search-hi-lock-overlays nil)))

(defun el-search-hi-lock-read-face-name ()
  (let ((defaults (append (list "el-search-hi-lock-decent-1"
                                "el-search-hi-lock-decent-2"
                                "el-search-hi-lock-decent-3"
                                "el-search-hi-lock-decent-4")
                          hi-lock-face-defaults)))
    (pcase-dolist (`(,_ ,_ ,_ ,(and face (let face-name (face-name face))))
                   (reverse el-search-hi-lock-current-patterns))
      (ignore face)
      (when (member face-name defaults)
        (cl-callf2 delete face-name defaults)
        (setcdr (last defaults) (cons face-name nil))))
    (intern (completing-read
	     (format "Highlight using face (default %s): "
		     (car defaults))
	     obarray 'facep t nil 'face-name-history defaults))))

;;;###autoload
(defun el-search-hi-lock-add-pattern (pattern face &optional priority pred)
  (interactive (list (el-search-read (el-search--read-pattern "Highlight pattern: "))
                     (el-search-hi-lock-read-face-name)))
  (cl-callf2 cons
      `(,pattern
        ,(el-search-make-matcher pattern)
        ,(el-search-heuristic-matcher pattern)
        ,face ,priority ,pred)
      el-search-hi-lock-current-patterns)
  (el-search-hi-lock-mode +1)
  (el-search-hi-lock-update-buffer-windows))

(defun el-search-hi-lock-remove-pattern (pattern)
  (interactive (list (el-search-read (completing-read "Remove highlighting of pattern: "
                                                      (mapcar (lambda (entry) (prin1-to-string (car entry)))
                                                              el-search-hi-lock-current-patterns)))))
  (setq el-search-hi-lock-current-patterns
        (delq nil (mapcar (lambda (entry) (if (equal (car entry) pattern) nil entry))
                          el-search-hi-lock-current-patterns)))
  (el-search-hi-lock-update-buffer-windows))



(provide 'el-search-hi-lock)
;;; el-search-hi-lock.el ends here
