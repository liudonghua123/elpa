;;; alarm-clock.el --- Simple alarm clock    -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 27 Aug 2018
;; Keywords: calendar
;; Compatibility: GNU Emacs 25
;; Version: 0.1
;; Package-Requires: ()


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

;; An alarm clock for Emacs.  Allows a bit more fine-tuning than
;; appt.el.
;;
;; Add a new alarm clock with M-x alarm-clock-add.  You are prompted
;; for a time or number of minutes and a name - e.g. enter "40" and
;; "Cake" meaning "Tell me after 40 minutes that I should take the
;; cake out of the hearth".  You get a countdown in the mode line
;; (unless you turn the option alarm-clock-display-in-mode-line off),
;; and a menu pops up when you click on it with mouse-3.  All other
;; commands can be performed from that menu.
;;
;; You can have any number of alarm clocks - but you can also specify
;; additional alarm times from the `alarm-clock-add' prompt for a
;; single clock, for situations like "Tell me in 40 minutes that my
;; cake is finished, but already after 30 minutes that I should reduce
;; the heat".
;;
;; There are (canonically named) commands to pause, resume and remove
;; an alarm clock (if a clock has multiple alarms, all of them are
;; affected at the same time).  If an alarm time has come, you get an
;; alarm.  Configure `alarm-clock-notify-function' to make that alarm
;; fit your needs - the default just `ding's and displays a message.
;;
;; If you want to keep expired alarm-clocks until you explicitly
;; remove them, configure `alarm-clock-autoremove ' to nil.  Expired
;; clocks get a red face in the mode-line.  Finally, adding clocks
;; with a running time of zero can serve as a simple kind of notes
;; with timestamps.
;;
;; Configure `alarm-clock-default-alarms-alist' to predefine alarm
;; clock name + duration associations.  These are available via
;; completion in `alarm-clock-add'.
;;
;; Last but not least, configure `alarm-clock-use-save-file' to
;; non-nil if you wish that alarm clocks survive Emacs restarts (or
;; crashes).  In this case, alarm clocks are automatically saved to
;; and restored from the file specified by
;; `alarm-clock-save-file-name'.  You want to add an
;; (alarm-clock-load) call to your init file in this case.  Since all
;; Emacs instances share the configured save file, you should not
;; manipulate alarm clocks in different Emacs instances in parallel.
;;
;;
;; TODO:
;;
;; - Manchmal klingelts obwohl clock gelöscht?
;;
;; - Add a menu



;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(require 'appt) ; appt-convert-time
(require 'seq)


(defgroup alarm-clock nil
  "Simple alarm clock"
  :group 'applications)

(defcustom alarm-clock-display-in-mode-line t
  "Doc..."
  :type 'boolean)

(defcustom alarm-clock-default-alarms-alist '()
  "Doc..."
  :type '(alist :key-type (string :tag "Name")
                :value-type (integer :tag "Minutes")))

(defface alarm-clock-in-mode-line
  '((((class color) (min-colors 88) (background dark))
     :foreground "SeaGreen" :height .8)
    (((class color) (min-colors 88) (background light))
     :foreground "DarkGreen" :height .8)
    (((class color) (min-colors 8) (background light))
     :foreground "green")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow"))
  "Face for displaying alarm clocks in the mode-line.
Only has an effect if `alarm-clock-display-in-mode-line' is
non-nil.")

(defface alarm-clock-paused-in-mode-line
  '((((class color) (min-colors 88))
     :foreground "RoyalBlue" :height .8)
    (((class color) (min-colors 8) (background light))
     :foreground "green")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow"))
  "Face for displaying paused alarm clocks in the mode-line.
Only has an effect if `alarm-clock-display-in-mode-line' is
non-nil.")

(defface alarm-clock-expired-in-mode-line
  '((t :foreground "red" :height .8))
  "Face for displaying expired alarm clocks in the mode-line.
Only used when `alarm-clock-autoremove' is nil.")

(defcustom alarm-clock-notify-function #'alarm-clock-default-notify-function
  "Alarm function called by alarm clocks.
It should accept one argument, an alarm message string."
  :type 'function )

(defcustom alarm-clock-autoremove t
  "Whether to automatically delete expired alarm clocks."
  :type 'boolean)

(defcustom alarm-clock-use-save-file nil
  "Doc..."
  :type 'boolean)

(defcustom alarm-clock-save-file-name (expand-file-name "emacs-alarm-clocks.el" user-emacs-directory)
  "Doc..."
  :type 'file)

(cl-defstruct alarm-clock
  name timers paused-p
  ;; NAME is a string.
  ;; An alarm clock and all of its alarms are either not paused; then
  ;; TIMERS is a list of timer objects whose `timer--time's are the
  ;; expiry times of the alarms and whose functions arrange to call the
  ;; value of `alarm-clock-notify-function', or all paused, in which case
  ;; TIMERS is nil and PAUSED-P is a sorted list of time differences.
  ;; An element of TIMERS can also be a TIME, meaning the specific alarm
  ;; had been expired at TIME.
  )

(defvar alarm-clock-list '()
  "List of currently active and paused alarm clocks.")

(defun alarm-clock-get (name-or-alarm-clock)
  "Get the alarm-clock object specified by NAME-OR-ALARM-CLOCK."
  (if (alarm-clock-p name-or-alarm-clock)
      name-or-alarm-clock
    (cl-some (lambda (c) (and (string= (alarm-clock-name c) name-or-alarm-clock) c))
             alarm-clock-list)))

(defun alarm-clock-times (clock)
  "Return a sorted list of finishing times of CLOCK."
  (sort
   (mapcar (lambda (timer-or-time)
             (if (timerp timer-or-time) (timer--time timer-or-time) timer-or-time))
           (alarm-clock-timers clock))
   #'time-less-p))

(defun alarm-clock-read-time (&optional prompt default)
  (let ((input
         ;; FIXME: factor out, generalize allowed syntax (seconds, ...)
         (read-string (concat (or prompt "Time (hh:mm[am/pm] or minutes from now): ")
                              ;; FIXME: two times ":" here
                              (if default (format " (default %s): " default) ""))
                      nil nil)))
    (cond
     ((string= input "") (if default `(delta ,(* 60 default)) nil))
     ((string-match "\\`[0-9]+\\'" input)
      (list 'delta (* 60 (string-to-number input))))
     (t (time-add
         (apply #'encode-time (apply #'list 0 0 0 (nthcdr 3 (decode-time (current-time)))))
         (* 60 (appt-convert-time input)))))))

(defun alarm-clock-default-notify-function (message)
  "Default notification function of alarm-clocks.

Only `ding's and `message'es the MESSAGE."
  (ding)
  (message "%s" message))

(defun alarm-clock--cancel-timers (clock)
  (mapc #'cancel-timer (seq-filter #'timerp (alarm-clock-timers clock)))
  (setf (alarm-clock-timers clock) '())
  clock)

(defun alarm-clock--remove-1 (clock)
  (alarm-clock--cancel-timers clock)
  (cl-callf2 delq clock alarm-clock-list)
  (alarm-clock--maybe-turn-off-mode-line-display)
  (alarm-clock-maybe-save))

(defun alarm-clock--notify-function (alarm-clock timer)
  (lambda ()
    (funcall alarm-clock-notify-function
             (format "Timer %s %salert"
                     (alarm-clock-name alarm-clock)
                     (let ((timers (alarm-clock-timers alarm-clock)))
                       (if (not (cdr timers)) ""
                         (let ((pos (1+ (seq-position timers timer))))
                           (pcase pos
                             (1 "first ")
                             (2 "second ")
                             (3 "third ")
                             (n (format "%dth " n))))))))
    (alarm-clock--maybe-turn-off-mode-line-display)
    (unless (or (not alarm-clock-autoremove)
                (cl-some (lambda (timer) (memq timer timer-list))
                         (alarm-clock-timers alarm-clock)))
      (alarm-clock--remove-1 alarm-clock))))

;;;###autoload
(defun alarm-clock-add (name times &optional paused)
  (interactive (let ((times '())
                     (default
                       (let ((n 65) name)
                         (while (progn (setq name (string n))
                                       (cl-some (apply-partially #'string= name)
                                                (mapcar #'alarm-clock-name alarm-clock-list)))
                           (cl-incf n))
                         name)))
                 (let ((name (completing-read
                              (format "Clock name (default \"%s\"): " default)
                              (mapcar #'car alarm-clock-default-alarms-alist))))
                   (when (string= name "") (setq name default))
                   (when (cl-some (apply-partially #'string= name)
                                  (mapcar #'alarm-clock-name alarm-clock-list))
                     ;; FIXME: give chance to reenter name
                     (user-error "An alarm clock named \"%s\" already exists" name))
                   (setq times (list (or (alarm-clock-read-time
                                          nil (cdr (assoc name alarm-clock-default-alarms-alist)))
                                         '(delta 0))))
                   ;; (while (and
                   ;;         (or (cdr times) (not (equal (car times) '(delta 0))))
                   ;;         (pcase (alarm-clock-read-time "Additional alarm at (leave empty for none): ")
                   ;;           ((and (pred identity) time) (push time times) t))))
                   (list name times current-prefix-arg))))
  (let ((new-alarm-clock
         (make-alarm-clock
          :name name
          :paused-p (let ((now (current-time)))
                      (sort (mapcar
                             (lambda (time)
                               (pcase time
                                 (`(delta ,delta) delta)
                                 (_ (time-subtract time now))))
                             times)
                            #'time-less-p)))))
    (add-to-list 'alarm-clock-list new-alarm-clock 'append)
    (unless paused (alarm-clock-resume new-alarm-clock))
    (when alarm-clock-display-in-mode-line
      (alarm-clock-in-mode-line-mode +1))
    (alarm-clock-maybe-save)
    new-alarm-clock))

(defvar alarm-clock-display-in-mode-line-timer nil)
(defvar alarm-clock-in-mode-line-string "")
(defvar alarm-clock-in-mode-line-interval 1)

(defun alarm-clock--maybe-turn-off-mode-line-display ()
  (when (alarm-clock--all-clocks-expired-p)
    (if (not alarm-clock-list)
        (alarm-clock-in-mode-line-mode -1)
      (when (timerp alarm-clock-display-in-mode-line-timer)
        (cancel-timer alarm-clock-display-in-mode-line-timer))
      (alarm-clock-in-mode-line-update))))

(defun alarm-clock-expired-p (clock)
  (not (or (cl-some (apply-partially #'< 0)
                    (mapcar #'time-to-seconds (alarm-clock-paused-p clock)))
           (cl-some (apply-partially #'time-less-p (current-time))
                    (alarm-clock-times clock)))))

(defun alarm-clock-some-subclock-expired-p (clock)
  (if (alarm-clock-paused-p clock)
      (not (cl-every (apply-partially #'< 0)
                     (mapcar #'time-to-seconds (alarm-clock-paused-p clock))))
    (not (cl-every (apply-partially #'time-less-p (current-time))
                   (alarm-clock-times clock)))))

(defun alarm-clock--all-clocks-expired-p ()
  (not (cl-some (lambda (clock) (not (alarm-clock-expired-p clock)))
                alarm-clock-list)))

(defun alarm-clock--format-seconds (seconds)
  "Format number SECONDS as [HH:]MM:SS."
  (if (< seconds 0)
      "-"
    (mapconcat
     (apply-partially #'format "%02d")
     (let* ((hours   (floor (/ seconds (* 60 60))))
            (minutes (floor (/ (- seconds (* hours (* 60 60)))
                               60)))
            (seconds (floor (- seconds
                               (* hours (* 60 60))
                               (* minutes 60))))
            (result (list hours minutes seconds)))
       (when (zerop (car result))
         (pop result))
       result)
     ":")))

(defun alarm-clock-times-string (alarm-clock)
  (if-let ((paused (alarm-clock-paused-p alarm-clock)))
      (mapconcat #'alarm-clock--format-seconds (mapcar #'time-to-seconds paused) "  ")
    (mapconcat (apply-partially #'format-time-string "<%T>")
               (alarm-clock-times alarm-clock)
               "  ")))

(defun alarm-clock-list-clocks ()
  "Print a list of all alarm clocks."
  (interactive)
  (message "It is now %s\n\n%s"
           (current-time-string)
           (if alarm-clock-list
               (mapconcat
                (lambda (alarm-clock)
                  (concat (alarm-clock-name alarm-clock) "  " (alarm-clock-times-string alarm-clock)))
                alarm-clock-list
                "\n")
             "no alarm clocks")))

(defun alarm-clock-in-mode-line-update ()
  (let ((pitch (propertize " " 'display '(space :width 1.5))))
    (setq alarm-clock-in-mode-line-string
          (apply #'list ""
                 (mapcar
                  (lambda (alarm-clock)
                    `(,pitch
                      (:propertize
                       ,(apply #'concat
                               (alarm-clock-name alarm-clock)
                               (unless (alarm-clock-expired-p alarm-clock)
                                 (list " "
                                       (mapconcat
                                        #'identity
                                        (let ((now (current-time)))
                                          (mapcar
                                           (lambda (time)
                                             (alarm-clock--format-seconds
                                              (time-to-seconds
                                               (if (alarm-clock-paused-p alarm-clock)
                                                   time
                                                 (time-subtract time now)))))
                                           (or (alarm-clock-paused-p alarm-clock)
                                               (alarm-clock-times alarm-clock))))
                                        "/"))))
                       mouse-face mode-line-highlight
                       face       ,(cond
                                    ((alarm-clock-expired-p alarm-clock)
                                     'alarm-clock-expired-in-mode-line)
                                    ((alarm-clock-paused-p alarm-clock)
                                     'alarm-clock-paused-in-mode-line)
                                    (t 'alarm-clock-in-mode-line))
                       help-echo
                       ,(concat (alarm-clock-times-string alarm-clock)
                                "\nmouse-3 for menu")
                       local-map ,(make-mode-line-mouse-map
                                   'mouse-3 (alarm-clock-mouse-3-fun alarm-clock)))))
                  alarm-clock-list))))
  (force-mode-line-update))

(define-minor-mode alarm-clock-in-mode-line-mode
  "Doc..."
  :global t
  (when (timerp alarm-clock-display-in-mode-line-timer)
    (cancel-timer alarm-clock-display-in-mode-line-timer))
  (setq alarm-clock-display-in-mode-line-timer nil)
  (setq alarm-clock-in-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (when alarm-clock-in-mode-line-mode
    (let ((mode-line-elt '(:eval alarm-clock-in-mode-line-string)))
      (unless (member mode-line-elt global-mode-string)
	(setq global-mode-string
	      (append global-mode-string (list mode-line-elt)))))
    (unless (alarm-clock--all-clocks-expired-p)
      (setq alarm-clock-display-in-mode-line-timer
	    (run-at-time t alarm-clock-in-mode-line-interval
		         'alarm-clock-in-mode-line-update)))
    (alarm-clock-in-mode-line-update)))

(defun alarm-clock-remove (name-or-alarm-clock &optional noquery)
  (interactive (list (completing-read "Remove clock: " (mapcar #'alarm-clock-name alarm-clock-list))
                     nil))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (when (or noquery
              (y-or-n-p
               (format "Remove alarm clock \"%s\"%s? "
                       (alarm-clock-name clock)
                       (let ((nbr-alarms (length (or (alarm-clock-paused-p clock)
                                                     (alarm-clock-timers clock)))))
                         (if (< 1 nbr-alarms) (format " and its %d alarms" nbr-alarms) "")))))
      (alarm-clock--remove-1 clock))))

(defun alarm-clock-pause (name-or-alarm-clock)
  (interactive (list (completing-read "Pause clock: " (mapcar #'alarm-clock-name alarm-clock-list))))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (if (alarm-clock-paused-p clock)
        (when (called-interactively-p 'any)
          (user-error "Already paused"))
      (let ((now (current-time)))
        (setf (alarm-clock-paused-p clock)
              (sort
               (mapcar (lambda (timer-or-time)
                         (time-subtract
                          (if (timerp timer-or-time)
                              (timer--time timer-or-time)
                            timer-or-time)
                          now))
                       (alarm-clock-timers clock))
               #'time-less-p)))
      (alarm-clock--cancel-timers clock)))
  (alarm-clock-maybe-save))

(defun alarm-clock--setup-timer (time clock)
  (let ((timer (timer-create)))
    (timer-set-time timer time)
    (timer-set-function timer (alarm-clock--notify-function clock timer))
    (timer-activate timer)
    (push timer (alarm-clock-timers clock))
    timer))

(defun alarm-clock-resume (name-or-alarm-clock)
  (interactive (list (completing-read "Resume clock: "
                                      (mapcar #'alarm-clock-name
                                              (seq-filter #'alarm-clock-paused-p alarm-clock-list)))))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (unless (alarm-clock-paused-p clock)
      (when (called-interactively-p 'any)
        (user-error "Not paused")))
    (dolist (time-delta (alarm-clock-paused-p clock))
      (let ((time (time-add (current-time) time-delta)))
        (if (time-less-p 0 time-delta)
            (alarm-clock--setup-timer time clock)
          (push time (alarm-clock-timers clock)))))
    (setf (alarm-clock-paused-p clock) nil)
    (alarm-clock-maybe-save)
    clock))

(defalias 'alarm-clock-start 'alarm-clock-resume)

(defun alarm-clock-rename (name-or-alarm-clock &optional new-name)
  (interactive (let ((clock-name (completing-read "Rename clock: "
                                                  (mapcar #'alarm-clock-name alarm-clock-list))))
                 (list clock-name (read-string "New name: " nil nil clock-name))))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (cl-callf or new-name (read-string "New name: " nil nil (alarm-clock-name clock)))
    (when (cl-some (apply-partially #'string= new-name)
                   (mapcar #'alarm-clock-name (remq clock alarm-clock-list)))
      (user-error "There is already a clock with that name"))
    (setf (alarm-clock-name clock) new-name))
  (alarm-clock-in-mode-line-update)
  (alarm-clock-maybe-save))

(defun alarm-clock--latest-time (clock)
  (car (nreverse (sort (alarm-clock-times clock) #'time-less-p))))

(defun alarm-clock-add-subclock (name-or-alarm-clock &optional minutes)
  (interactive
   (list (completing-read "Add subtimer to clock: " (mapcar #'alarm-clock-name alarm-clock-list))))
  (cl-callf or minutes (string-to-number (read-string "Minutes before last alarm: ")))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (if-let ((paused (alarm-clock-paused-p clock)))
        (progn
          (push (time-subtract (apply #'max (mapcar #'time-to-seconds paused))
                               (seconds-to-time (* 60 minutes)))
                (alarm-clock-paused-p clock))
          (cl-callf sort (alarm-clock-paused-p clock) #'time-less-p))
      (alarm-clock--setup-timer (time-subtract (alarm-clock--latest-time clock) (* 60 minutes)) clock)
      (cl-callf cl-sort (alarm-clock-timers clock) #'time-less-p
                :key (lambda (timer-or-time)
                       (if (timerp timer-or-time) (timer--time timer-or-time) timer-or-time)))))
  (alarm-clock-maybe-save))

(defun alarm-clock-splice (name-or-alarm-clock)
  (interactive (list (completing-read "Splice clock: "
                                      (mapcar #'alarm-clock-name
                                              (seq-filter #'alarm-clock-paused-p alarm-clock-list)))))
  (let* ((clock  (alarm-clock-get name-or-alarm-clock))
         (oname (alarm-clock-name clock))
         (was-paused (alarm-clock-paused-p clock)))
    (unless was-paused (alarm-clock-pause clock))
    (let ((times (seq-filter (lambda (time) (< 0 (time-to-seconds time)))
                             (alarm-clock-paused-p clock))))
      (if (not (cdr times))
          (if (called-interactively-p 'any)
              (user-error "That doesn't make much sense")
            (unless was-paused (alarm-clock-resume clock)))
        (alarm-clock-remove clock 'noquery)
        (let ((i (1+ (length times))))
          (dolist (time (nreverse times))
            (alarm-clock-add (format "%s%d" oname (cl-decf i))
                             (list `(delta ,(time-to-seconds time)))
                             was-paused))))))
  (alarm-clock-maybe-save))

(defun alarm-clock-remove-expired-subclocks (name-or-alarm-clock)
  "Doc..."
  (interactive
   (list (completing-read "Remove expired subclocks of clock: "
                          (mapcar #'alarm-clock-name alarm-clock-list))))
  (let ((clock (alarm-clock-get name-or-alarm-clock)))
    (if (alarm-clock-paused-p clock)
        (setf (alarm-clock-paused-p clock)
              (seq-filter
               (lambda (time) (< 0 (time-to-seconds time)))
               (alarm-clock-paused-p clock)))
      (let ((current-time (current-time)))
        (setf (alarm-clock-timers clock)
              (seq-filter
               (lambda (timer-or-time)
                 (time-less-p current-time
                              (if (timerp timer-or-time) (timer--time timer-or-time) timer-or-time)))
               (alarm-clock-timers clock)))))
    clock))

(defun alarm-clock-remove-subclock (name-or-alarm-clock)
  "Doc..."
  (interactive
   (list (completing-read "Remove a subclock of clock: "
                          (mapcar #'alarm-clock-name alarm-clock-list))))
  (let* ((clock (alarm-clock-get name-or-alarm-clock))
         (paused (alarm-clock-paused-p clock))
         (subs (or paused (cl-sort (copy-sequence (alarm-clock-timers clock))
                                   #'time-less-p :key #'timer--time))))
    (unless (cdr subs)
      (user-error "Clock only has one alarm"))
    (let* ((completion-extra-properties
            (list :annotation-function
                  (lambda (s) (let ((n (1- (string-to-number s))))
                                (concat "  "
                                        (alarm-clock--format-seconds
                                         (time-to-seconds
                                          (if paused (nth n paused)
                                            (let ((current-time (current-time)))
                                              (time-subtract
                                               (timer--time (nth n subs))
                                               current-time))))))))))
           (n (1- (string-to-number
                   (completing-read "Delete subclock number: "
                                    (mapcar #'number-to-string (number-sequence 1 (length subs)))))))
           (new-subs (delq nil (seq-map-indexed
                                (lambda (elt idx)
                                  (if (/= idx n)
                                      elt
                                    (when (timerp elt) (cancel-timer elt))
                                    nil))
                                subs))))
      (if paused
          (setf (alarm-clock-paused-p clock) new-subs)
        (setf (alarm-clock-timers clock) new-subs)))))

(defun alarm-clock-mouse-3-fun (clock)
  (lambda ()
    (interactive)
    (popup-menu
     `(,(format "Alarm %s" (alarm-clock-name clock))
       (keymap
        ;; FIXME: Add echo help texts
        ,(vector "Pause"  (lambda () (interactive) (alarm-clock-pause clock))
                 :enable `(and (not (alarm-clock-paused-p ,clock))
                               (cl-some
                                (apply-partially #'time-less-p (current-time))
                                (mapcar #'timer--time
                                        (seq-filter #'timerp (alarm-clock-timers ,clock)))))
                 :help "Pause alarms for this clock")
        ,(vector "Resume" (lambda () (interactive) (alarm-clock-resume clock))
                 :enable `(alarm-clock-paused-p ,clock)
                 :help "Resume clock")
        ,(vector "Remove" (lambda () (interactive) (alarm-clock-remove clock))
                 :help "Remove clock and all of its alarms")
        ,(vector "Rename" (lambda () (interactive) (alarm-clock-rename clock))
                 :help "Rename this clock")
        ,(vector "Splice" (lambda () (interactive) (alarm-clock-splice clock))
                 :enable `(cdr (or (alarm-clock-timers ,clock)
                                   (alarm-clock-paused-p ,clock)))
                 :help "Splice alarms of this clock into independent clocks")
        ,(vector "Add Alarm" (lambda () (interactive) (alarm-clock-add-subclock clock))
                 :help "Add alarm for this clock"
                 :enable `(not (alarm-clock-expired-p ,clock)))
        ,(vector "Remove expired subclocks"
                 (lambda () (interactive) (alarm-clock-remove-expired-subclocks clock))
                 :help "Remove all expired subclocks of this clock"
                 :enable `(and (not (alarm-clock-expired-p ,clock))
                               (alarm-clock-some-subclock-expired-p ,clock)))
        ,(vector "Remove subclock"
                 (lambda () (interactive) (alarm-clock-remove-subclock clock))
                 :help "Remove a subclock from this clock"
                 :enable `(cdr (or (alarm-clock-paused-p ,clock) (alarm-clock-timers ,clock)))))))))

(defun alarm-clock-maybe-save ()
  "Save alarm clocks in `alarm-clock-save-file-name'."
  (when alarm-clock-use-save-file
    (let ((old-buffer (find-buffer-visiting alarm-clock-save-file-name)))
      (with-current-buffer (or old-buffer
                               (let ((delay-mode-hooks t))
                                 (find-file-noselect alarm-clock-save-file-name)))
        (let ((inhibit-read-only t)
	      (print-length nil)
	      (print-level nil))
          (let ((standard-output (current-buffer)))
            (erase-buffer)
            (dolist (c alarm-clock-list)
              (prin1 `(alarm-clock-add ,(alarm-clock-name c)
                                       ,@(if-let ((paused (alarm-clock-paused-p c)))
                                             `(',(mapcar (lambda (x) `(delta ,x)) paused) t)
                                           `(',(alarm-clock-times c)))))
              (princ "\n"))))
        (let ((file-precious-flag t)
              (inhibit-message t)
              (message-log-max nil))
          (save-buffer))
        (unless old-buffer (kill-buffer (current-buffer)))))))

;;;###autoload
(defun alarm-clock-load ()
  ;; Only does something when alarm-clock-list -> nil
  (when (and alarm-clock-use-save-file
             (file-readable-p alarm-clock-save-file-name)
             (not alarm-clock-list))
    (condition-case err
        (let ((alarm-clock-use-save-file nil))
          (load-file alarm-clock-save-file-name))
      (error (message "Error while loading %s: %s"
                      alarm-clock-save-file-name
                      (error-message-string err))
             (sit-for 2)))))


(provide 'alarm-clock)


;;; alarm-clock.el ends here
