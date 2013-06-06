;;; File to make aliases
;;; Ritter & Nichols
;;; Nov. 1994
;;;
;;; A simple way to create aliases using command name acronyming for 
;;; multi-word commands, and truncation otherwise.
;;;
;;;
;;; Table of contents
;;;
;;;	i.	Variables and initializations
;;;	I.	make-alias
;;;	II.	Some testing code.


;;;
;;;	i.	Variables and initializations
;;;

(defvar *old-aliases* nil "*Old commands that you have passed in.")

(defvar *new-aliases* nil "*Aliases that you have created.") 

(defun  *dup-alises* nil "*Where possible clashes are stored.")

(setq dis-user-cell-functions 
      (cons 'init-make-aliases dis-user-cell-functions))

(defun init-make-aliases ()
  "Clears out the global variables for make-aliases."
  (interactive)
  (setq *old-aliases* nil)  
  (setq *new-aliases* nil)
  (setq *dup-alises* nil))


;;;
;;;	I.	make-alias
;;;

(setq dis-user-cell-functions (cons 'make-alias dis-user-cell-functions))

(defun make-alias (old) 
  "Makes an alias given an OLD command."
  ;; check that old command is a string
  (if (not (stringp old)) (error "%s not a string" old))
  (setq *old-aliases* (nconc *old-aliases* old))  
  (let (new)
     (cond ((> (length old) 5)
            ;; if long, take first letter of each word
            (let ((count 0))
              (setq new (substring old 0 1))
              (while (< count (length old))
                (if (string= "-" (substring old count (1+ count)))
                    (setq new (concat new (substring old (1+ count) 
                                                     (+ count 2)))))
		    (setq count (1+ count)))
	       (if (eq (length new) 1)
		   (setq new (concat new (substring old 1 2))))))
           ;; if short, just take first letter
	   (t (setq new (substring old 0 1))))
  ;; keep track of duplicate aliases
  (if (member new *new-aliases*) (setq *dup-alises* (cons new *dup-aliases*)))
  (setq *new-aliases* (cons new *new-aliases*))
  ;; return the alias
  new))


;;; 
;;;	II.	Some testing code.
;;;

;; (init-make-aliases)
;; (make-alias "chunk-free-prblem-spaces")
;; *old-aliases*
;; *new-aliases*
;; *dup-aliases*





