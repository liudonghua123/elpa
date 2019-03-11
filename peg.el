;;; peg.el --- Parsing Expression Grammars in Emacs Lisp  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2019  Free Software Foundation, Inc.
;;
;; Author: Helmut Eller <eller.helmut@gmail.com>
;; Maintainer: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.7
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Parsing Expression Grammars (PEG) are a formalism in the spirit of
;; Context Free Grammars (CFG) with some simplifications which makes
;; the implementation of PEGs as recursive descent parser particularly
;; simple and easy to understand [Ford, Baker].
;;
;; This file implements a macro `peg-parse' which parses the current
;; buffer according to a PEG.  E.g. we can match integers with a PEG
;; like this:
;;
;;  (peg-parse (number   sign digit (* digit))
;;             (sign     (or "+" "-" ""))
;;             (digit    [0-9]))
;;
;; In contrast to regexps, PEGs allow us to define recursive "rules".
;; A "grammar" is a list of rules.  A rule is written as (NAME PEX...)
;; E.g. (sign (or "+" "-" "")) is a rule with the name "sign".  The
;; syntax for PEX (Parsing Expression) is a follows:
;;
;; Description		Lisp		Traditional, as in Ford's paper
;; Sequence		(and e1 e2)	e1 e2
;; Prioritized Choice   (or e1 e2)	e1 / e2
;; Not-predicate	(not e)		!e
;; And-predicate	(if e)		&e
;; Any character	(any)		.
;; Literal string	"abc"		"abc"
;; Character C		(char c)	'c'
;; Zero-or-more		(* e)		e*
;; One-or-more		(+ e)		e+
;; Optional		(opt e)		e?
;; Character range	(range a b)	[a-b]
;; Character set	[a-b "+*" ?x]	[a-b+*x]  ; note: [] is a elisp vector
;; Character classes    [ascii cntrl]
;; Beginning-of-Buffer  (bob)
;; End-of-Buffer        (eob)
;; Beginning-of-Line    (bol)
;; End-of-Line		(eol)
;; Beginning-of-Word    (bow)
;; End-of-Word		(eow)
;; Beginning-of-Symbol  (bos)
;; End-of-Symbol	(eos)
;; Syntax-Class		(syntax-class NAME)
;;
;; `peg-parse' also supports parsing actions, i.e. Lisp snippets which
;; are executed when a pex matches.  This can be used to construct
;; syntax trees or for similar tasks.  Actions are written as
;;
;;  (action FORM)          ; evaluate FORM
;;  `(VAR... -- FORM...)   ; stack action
;;
;; Actions don't consume input, but are executed at the point of
;; match.  A "stack action" takes VARs from the "value stack" and
;; pushes the result of evaluating FORMs to that stack.  See
;; `peg-ex-parse-int' for an example.
;;
;; Derived Operators:
;;
;; The following operators are implemented as combinations of
;; primitive expressions:
;;
;; (substring E)  ; match E and push the substring for the matched region
;; (region E)     ; match E and push the corresponding start and end positions
;; (replace E RPL); match E and replace the matched region with RPL.
;; (list E)       ; match E and push a list out of the items that E produces.
;;
;; Regexp equivalents:
;;
;; Here a some examples for regexps and how those could be written as pex.
;; [Most are taken from rx.el]
;;
;; "^[a-z]*"
;; (and (bol) (* [a-z]))
;;
;; "\n[^ \t]"
;; (and "\n" (not [" \t"]) (any))
;;
;; "\\*\\*\\* EOOH \\*\\*\\*\n"
;; "*** EOOH ***\n"
;;
;; "\\<\\(catch\\|finally\\)\\>[^_]"
;; (and (bow) (or "catch" "finally") (eow) (not "_") (any))
;;
;; "[ \t\n]*:\\([^:]+\\|$\\)"
;; (and (* [" \t\n"]) ":" (or (+ (not ":") (any)) (eol)))
;;
;; "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
;; (and (bol)
;;      "content-transfer-encoding:"
;;      (* (opt "\n") ["\t "])
;;      "quoted-printable"
;;      (* (opt "\n") ["\t "]))
;;
;; "\\$[I]d: [^ ]+ \\([^ ]+\\) "
;; (and "$Id: " (+ (not " ") (any)) " " (+ (not " ") (any)) " ")
;;
;; "^;;\\s-*\n\\|^\n"
;; (or (and (bol) ";;" (* (syntax-class whitespace)) "\n")
;;     (and (bol) "\n"))
;;
;; "\\\\\\\\\\[\\w+"
;; (and "\\\\[" (+ (syntax-class word)))
;;
;; Search forward for ";;; Examples" for other examples.
;;
;; References:
;;
;; [Ford] Bryan Ford. Parsing Expression Grammars: a Recognition-Based
;; Syntactic Foundation. In POPL'04: Proceedings of the 31st ACM
;; SIGPLAN-SIGACT symposium on Principles of Programming Languages,
;; pages 111-122, New York, NY, USA, 2004. ACM Press.
;; http://pdos.csail.mit.edu/~baford/packrat/
;;
;; [Baker] Baker, Henry G. "Pragmatic Parsing in Common Lisp".  ACM Lisp
;; Pointers 4(2), April--June 1991, pp. 3--15.
;; http://home.pipeline.com/~hbaker1/Prag-Parse.html
;;
;; Roman Redziejowski does good PEG related research
;; http://www.romanredz.se/pubs.htm

;;; Code:

(eval-when-compile (require 'cl-lib))

(defmacro peg-parse (&rest rules)
  "Match RULES at point.
Return (T STACK) if the match succeed and nil on failure."
  (peg-translate-rules rules))

(defmacro peg-parse-exp (exp)
  "Match the parsing expression EXP at point.
Note: a PE can't \"call\" rules by name."
  `(let ((peg--actions nil))
     (when ,(peg-translate-exp (peg-normalize exp))
       (peg-postprocess peg--actions))))

;; A table of the PEG rules.  Used during compilation to resolve
;; references to named rules.
(defvar peg--rules)

(defvar peg--actions nil
  "Actions collected along the current parse.
Used at runtime for backtracking.  It's a list ((POS . THUNK)...).
Each THUNK is executed at the corresponding POS.  Thunks are
executed in a postprocessing step, not during parsing.")

;; used at runtime to track the right-most error location.  It's a
;; pair (POSITION . EXPS ...).  POSITION is the buffer position and
;; EXPS is a list of rules/expressions that failed.
(defvar peg--errors)

(define-error 'peg-void-rule "Reference to undefined PEG rule: %S")

(defun peg--lookup-rule (name)
  (or (gethash name peg--rules)
      (signal 'peg-void-rule (list name))))

(defun peg--rule-var (name)
  (intern (format "peg--rule-%s" name)))

;; The basic idea is to translate each rule to a lisp function.
;; The result looks like
;;   (let ((rule1 (lambda () code-for-rule1))
;;         ...
;;         (ruleN (lambda () code-for-ruleN)))
;;     (funcall rule1))
;;
;; code-for-ruleX returns t if the rule matches and nil otherwise.
;;
(defun peg-translate-rules (rules)
  "Translate the PEG RULES, to a top-down parser."
  (let ((peg--rules (make-hash-table :size 20)))
    (dolist (rule rules)
      (puthash (car rule) (peg-normalize `(and . ,(cdr rule))) peg--rules))
    (peg-check-cycles)
    `(progn
       (defvar peg--errors) (defvar peg--actions)
       (let ((peg--actions '()) (peg--errors '(-1)))
         (letrec
             ,(mapcar (lambda (rule)
		        (let ((name (car rule)))
		          `(,(peg--rule-var name)
			    (lambda ()
			      ,(peg-translate-exp (gethash name peg--rules))))))
		      rules)
           (cond ((funcall ,(peg--rule-var (car (car rules))))
	          (peg-postprocess peg--actions))
	         (t
	          (goto-char (car peg--errors))
	          (error "Parse error at %d (expecting %S)"
		         (car peg--errors)
		         (peg-merge-errors (cdr peg--errors))))))))))


(eval-and-compile
  (defun peg-method-table-name (method-name)
    (intern (format "peg-%s-methods" method-name))))

;; FIXME: Replace peg-define-method-table/peg-add-method with cl-defgeneric and
;; cl-defmethod?
(defmacro peg-define-method-table (name)
  (let ((tab (peg-method-table-name name)))
    `(progn
       (defvar ,tab)
       (setq ,tab (make-hash-table :size 20)))))

(defmacro peg-add-method (method type args &rest body)
  (declare (indent 3)
           (debug (symbolp symbolp sexp def-body)))
  `(puthash ',type (lambda ,args . ,body) ,(peg-method-table-name method)))

(peg-define-method-table normalize)

;; Internally we use a regularized syntax, e.g. we only have binary OR
;; nodes.  Regularized nodes are lists of the form (OP ARGS...).
(defun peg-normalize (exp)
  "Return a \"normalized\" form of EXP."
  (cond ((and (consp exp)
	      (let ((fun (gethash (car exp) peg-normalize-methods)))
		(and fun
		     (apply fun (cdr exp))))))
	((stringp exp)
	 (let ((len (length exp)))
	   (cond ((zerop len) '(null))
		 ((= len 1) `(char ,(aref exp 0)))
		 (t `(str ,exp)))))
	((and (symbolp exp) exp)
         ;; (peg--lookup-rule exp)
	 `(call ,exp))
	((vectorp exp)
	 (peg-normalize `(set . ,(append exp '()))))
	(t
	 (error "Invalid parsing expression: %S" exp))))

(defvar peg-leaf-types '(null fail any call action char range str set
			      bob eob bol eol bow eow bos eos syntax-class =))

(dolist (type peg-leaf-types)
  (puthash type `(lambda (&rest args) (cons ',type args))
	   peg-normalize-methods))

(peg-add-method normalize or (&rest args)
  (cond ((null args) '(fail))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(or ,(peg-normalize (car args))
		,(peg-normalize `(or . ,(cdr args)))))))

(peg-add-method normalize and (&rest args)
  (cond ((null args) '(null))
	((null (cdr args)) (peg-normalize (car args)))
	(t `(and ,(peg-normalize (car args))
		 ,(peg-normalize `(and . ,(cdr args)))))))

(peg-add-method normalize * (&rest args)
  `(* ,(peg-normalize `(and . ,args))))

;; FIXME: this duplicates code; could use some loop to avoid that
(peg-add-method normalize + (&rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(and ,e (* ,e))))

(peg-add-method normalize opt (&rest args)
  (let ((e (peg-normalize `(and . ,args))))
    `(or ,e (null))))

(peg-add-method normalize if (&rest args)
  `(if ,(peg-normalize `(and . ,args))))

(peg-add-method normalize not (&rest args)
  `(not ,(peg-normalize `(and . ,args))))

(peg-add-method normalize \` (form)
  (peg-normalize `(stack-action ,form)))

(peg-add-method normalize stack-action (form)
  (unless (member '-- form)
    (error "Malformed stack action: %S" form))
  (let ((args (cdr (member '-- (reverse form))))
	(values (cdr (member '-- form))))
    (let ((form `(let ,(mapcar (lambda (var) `(,var (pop peg--stack))) args)
		   ,@(mapcar (lambda (val) `(push ,val peg--stack)) values))))
      `(action ,form))))

(defvar peg-char-classes
  '(ascii alnum alpha blank cntrl digit graph lower multibyte nonascii print
	  punct space unibyte upper word xdigit))

(peg-add-method normalize set (&rest specs)
  (cond ((null specs) '(fail))
	((and (null (cdr specs))
	      (let ((range (peg-range-designator (car specs))))
		(and range `(range ,(car range) ,(cdr range))))))
	(t
	 (let ((chars '()) (ranges '()) (classes '()))
	   (while specs
	     (let* ((spec (pop specs))
		    (range (peg-range-designator spec)))
	       (cond (range
		      (push range ranges))
		     ((peg-characterp spec)
		      (push spec chars))
		     ((stringp spec)
		      (setq chars (append (reverse (append spec ())) chars)))
		     ((memq spec peg-char-classes)
		      (push spec classes))
		     (t (error "Invalid set specifier: %S" spec)))))
	   (setq ranges (reverse ranges))
	   (setq chars (delete-dups (reverse chars)))
	   (setq classes (reverse classes))
	   (cond ((and (null ranges)
		       (null classes)
		       (cond ((null chars) '(fail))
			     ((null (cdr chars)) `(char ,(car chars))))))
		 (t `(set ,ranges ,chars ,classes)))))))

(defun peg-range-designator (x)
  (and (symbolp x)
       (let ((str (symbol-name x)))
	 (and (= (length str) 3)
	      (eq (aref str 1) ?-)
	      (< (aref str 0) (aref str 2))
	      (cons (aref str 0) (aref str 2))))))

;; characterp is new in Emacs 23.
(defun peg-characterp (x)
  (if (fboundp 'characterp)
      (characterp x)
    (integerp x)))

(peg-add-method normalize list (&rest args)
  (peg-normalize
   (let ((marker (make-symbol "magic-marker")))
     `(and (stack-action (-- ',marker))
	   ,@args
	   (stack-action (--
			  (let ((l '()))
			    (while
				(let ((e (pop peg--stack)))
				  (cond ((eq e ',marker) nil)
					((null peg--stack)
					 (error "No marker on stack"))
					(t (push e l) t))))
			    l)))))))

(peg-add-method normalize substring (&rest args)
  (peg-normalize
   `(and `(-- (point))
	 ,@args
	 `(start -- (buffer-substring-no-properties start (point))))))

(peg-add-method normalize region (&rest args)
  (peg-normalize
   `(and `(-- (point))
	 ,@args
	 `(-- (point)))))

(peg-add-method normalize replace (pe replacement)
  (peg-normalize
   `(and (stack-action (-- (point)))
	 ,pe
	 (stack-action (start -- (progn
				   (delete-region start (point))
				   (insert-before-markers ,replacement))))
	 (stack-action (_ --)))))

(peg-add-method normalize quote (_form)
  (error "quote is reserved for future use"))

(peg-define-method-table translate)

;; This is the main translation function.
(defun peg-translate-exp (exp)
  "Return the ELisp code to match the PE EXP."
  (let ((translator (or (gethash (car exp) peg-translate-methods)
			(error "No translator for: %S" (car exp)))))
    ;; FIXME: This expansion basically duplicates `exp' in the output, which is
    ;; a serious problem because it's done recursively, so it makes the output
    ;; code's size exponentially larger than the input!
    `(or ,(apply translator (cdr exp))
	 (progn
	   (peg-record-failure ',exp) ; for error reporting
	   nil))))

(defun peg-record-failure (exp)
  (cond ((= (point) (car peg--errors))
	 (setcdr peg--errors (cons exp (cdr peg--errors))))
	((> (point) (car peg--errors))
	 (setq peg--errors (list (point) exp)))))

(peg-add-method translate and (e1 e2)
  `(and ,(peg-translate-exp e1)
	,(peg-translate-exp e2)))

;; Choicepoints are used for backtracking.  At a choicepoint we save
;; enough state, so that we can continue from there if needed.
(defun peg--choicepoint-moved-p (choicepoint)
  `(/= ,(car choicepoint) (point)))
 
(defun peg--choicepoint-restore (choicepoint)
  `(progn
     (goto-char ,(car choicepoint))
     (setq peg--actions ,(cdr choicepoint))))

(defmacro peg--with-choicepoint (var &rest body)
  (declare (indent 1) (debug (symbolp form)))
  `(let ((,var (cons (make-symbol "point") (make-symbol "actions"))))
     `(let ((,(car ,var) (point))
	    (,(cdr ,var) peg--actions))
        ,@(list ,@body))))
 
(peg-add-method translate or (e1 e2)
  (peg--with-choicepoint cp
    `(or ,(peg-translate-exp e1)
	 (,@(peg--choicepoint-restore cp)
	  ,(peg-translate-exp e2)))))

;; match empty strings
(peg-add-method translate null ()
  `t)

;; match nothing
(peg-add-method translate fail ()
  `nil)

(peg-add-method translate bob () '(bobp))
(peg-add-method translate eob () '(eobp))
(peg-add-method translate eol () '(eolp))
(peg-add-method translate bol () '(bolp))
(peg-add-method translate bow () '(looking-at "\\<"))
(peg-add-method translate eow () '(looking-at "\\>"))
(peg-add-method translate bos () '(looking-at "\\_<"))
(peg-add-method translate eos () '(looking-at "\\_>"))

(defvar peg-syntax-classes
  '((whitespace ?-) (word ?w) (symbol ?s) (punctuation ?.)
    (open ?\() (close ?\)) (string ?\") (escape ?\\) (charquote ?/)
    (math ?$) (prefix ?') (comment ?<) (endcomment ?>)
    (comment-fence ?!) (string-fence ?|)))

(peg-add-method translate syntax-class (class)
  (let ((probe (assoc class peg-syntax-classes)))
    (cond (probe `(looking-at ,(format "\\s%c" (cadr probe))))
	  (t (error "Invalid syntax class: %S\nMust be one of: %s" class
		    (mapcar #'car peg-syntax-classes))))))

(peg-add-method translate = (string)
  `(let ((str ,string))
     (when (zerop (length str))
       (error "Empty strings not allowed for ="))
     (search-forward str (+ (point) (length str)) t)))

(peg-add-method translate * (e)
  `(progn (while ,(peg--with-choicepoint cp
		    `(if ,(peg-translate-exp e)
                         ;; Just as regexps do for the `*' operator,
                         ;; we allow the body of `*' loops to match
                         ;; the empty string, but we don't repeat the loop if
                         ;; we haven't moved, to avoid inf-loops.
                         ,(peg--choicepoint-moved-p cp)
                       ,(peg--choicepoint-restore cp)
		       nil)))
	  t))

(peg-add-method translate if (e)
  (peg--with-choicepoint cp
    `(when ,(peg-translate-exp e)
       ,(peg--choicepoint-restore cp)
       t)))

(peg-add-method translate not (e)
  (peg--with-choicepoint cp
    `(unless ,(peg-translate-exp e)
       ,(peg--choicepoint-restore cp)
       t)))

(peg-add-method translate any ()
  '(when (not (eobp))
     (forward-char)
     t))

(peg-add-method translate char (c)
  `(when (eq (char-after) ',c)
     (forward-char)
     t))

(peg-add-method translate set (ranges chars classes)
  `(when (looking-at ',(peg-make-charset-regexp ranges chars classes))
     (forward-char)
     t))

(defun peg-make-charset-regexp (ranges chars classes)
  (when (and (not ranges) (not classes) (<= (length chars) 1))
    (error "Bug"))
  (let ((rbracket (member ?\] chars))
	(minus (member ?- chars))
	(hat (member ?^ chars)))
    (dolist (c '(?\] ?- ?^))
      (setq chars (remove c chars)))
    (format "[%s%s%s%s%s%s]"
	    (if rbracket "]" "")
	    (if minus "-" "")
	    (mapconcat (lambda (x) (format "%c-%c" (car x) (cdr x))) ranges "")
	    (mapconcat (lambda (c) (format "[:%s:]" c)) classes "")
	    (mapconcat (lambda (c) (format "%c" c)) chars "")
	    (if hat "^" ""))))

(peg-add-method translate range (from to)
  `(when (and (char-after)
	      (<= ',from (char-after))
	      (<= (char-after) ',to))
     (forward-char)
     t))

(peg-add-method translate str (str)
  `(when (looking-at ',(regexp-quote str))
     (goto-char (match-end 0))
     t))

(peg-add-method translate call (name)
  (peg--lookup-rule name) ;; Signal error if not found!
  `(funcall ,(peg--rule-var name)))

(peg-add-method translate action (form)
  `(progn
     (push (cons (point) (lambda () ,form)) peg--actions)
     t))

(defvar peg--stack nil)
(defun peg-postprocess (actions)
  "Execute \"actions\"."
  (let  ((peg--stack '()))
    (pcase-dolist (`(,pos . ,thunk)
                   (mapcar (lambda (x)
			     (cons (copy-marker (car x)) (cdr x)))
			   (reverse actions)))
      (goto-char pos)
      (funcall thunk))
    peg--stack))

;; Left recursion is presumably a common mistake when using PEGs.
;; Here we try to detect such mistakes.  Essentailly we traverse the
;; graph as long as we can without consuming input.  When we find a
;; recursive call we signal an error.

(defun peg-check-cycles ()
  (maphash (lambda (name exp)
	     (peg-detect-cycles exp (list name))
	     (dolist (node (peg-find-star-nodes exp))
	       (peg-detect-cycles node '())))
	   peg--rules))

(defun peg-find-star-nodes (exp)
  (let ((type (car exp)))
    (cond ((memq type peg-leaf-types) '())
	  (t (let ((kids (apply #'append
				(mapcar #'peg-find-star-nodes (cdr exp)))))
	       (if (eq type '*)
		   (cons exp kids)
		 kids))))))

(peg-define-method-table detect-cycles)

(defun peg-detect-cycles (exp path)
  "Signal an error on a cycle.
Otherwise traverse EXP recursively and return T if EXP can match
without consuming input.  Return nil if EXP definetly consumes
input.  PATH is the list of rules that we have visited so far."
  (apply (or (gethash (car exp) peg-detect-cycles-methods)
	     (error "No detect-cycle method for: %S" exp))
	 path (cdr exp)))

(peg-add-method detect-cycles call (path name)
  (cond ((member name path)
	 (error "Possible left recursion: %s"
		(mapconcat (lambda (x) (format "%s" x))
			   (reverse (cons name path)) " -> ")))
	(t
	 (peg-detect-cycles (peg--lookup-rule name) (cons name path)))))

(peg-add-method detect-cycles and (path e1 e2)
  (and (peg-detect-cycles e1 path)
       (peg-detect-cycles e2 path)))

(peg-add-method detect-cycles or (path e1 e2)
  (or (peg-detect-cycles e1 path)
      (peg-detect-cycles e2 path)))

(peg-add-method detect-cycles * (path e)
  (peg-detect-cycles e path)
  t)

(peg-add-method detect-cycles if  (path e) (peg-unary-nullable e path))
(peg-add-method detect-cycles not (path e) (peg-unary-nullable e path))

(defun peg-unary-nullable (exp path)
  (peg-detect-cycles exp path)
  t)

(peg-add-method detect-cycles any   (_path)          nil)
(peg-add-method detect-cycles char  (_path _c)       nil)
(peg-add-method detect-cycles set   (_path _r _c _k) nil)
(peg-add-method detect-cycles range (_path _c1 _c2)  nil)
(peg-add-method detect-cycles str   (_path s)        (equal s ""))
(peg-add-method detect-cycles null  (_path)          t)
(peg-add-method detect-cycles fail  (_path)          nil)
(peg-add-method detect-cycles bob   (_path)          t)
(peg-add-method detect-cycles eob   (_path)          t)
(peg-add-method detect-cycles bol   (_path)          t)
(peg-add-method detect-cycles eol   (_path)          t)
(peg-add-method detect-cycles bow   (_path)          t)
(peg-add-method detect-cycles eow   (_path)          t)
(peg-add-method detect-cycles bos   (_path)          t)
(peg-add-method detect-cycles eos   (_path)          t)
(peg-add-method detect-cycles =     (_path _s)       nil)
(peg-add-method detect-cycles syntax-class (_p _n)   nil)
(peg-add-method detect-cycles action (_path _form)   t)

(peg-define-method-table merge-error)

(defun peg-merge-errors (exps)
  "Build a more readable error message out of failed expression."
  (let ((merged '()))
    (dolist (exp exps)
      (setq merged (peg-merge-error exp merged)))
    merged))

(defun peg-merge-error (exp merged)
  (apply (or (gethash (car exp) peg-merge-error-methods)
	     (error "No merge-error method for: %S" exp))
	 merged (cdr exp)))

(peg-add-method merge-error or (merged e1 e2)
  (peg-merge-error e2 (peg-merge-error e1 merged)))

(peg-add-method merge-error and (merged e1 _e2)
  ;; FIXME: Why is `e2' not used?
  (peg-merge-error e1 merged))

(peg-add-method merge-error str (merged str)
  ;;(add-to-list 'merged str)
  (cl-adjoin str merged :test #'equal))

(peg-add-method merge-error call (merged rule)
  ;; (add-to-list 'merged rule)
  (cl-adjoin rule merged :test #'equal))

(peg-add-method merge-error char (merged char)
  ;; (add-to-list 'merged (string char))
  (cl-adjoin (string char) merged :test #'equal))

(peg-add-method merge-error set (merged r c k)
  ;; (add-to-list 'merged (peg-make-charset-regexp r c k))
  (cl-adjoin (peg-make-charset-regexp r c k) merged :test #'equal))

(peg-add-method merge-error range (merged from to)
  ;; (add-to-list 'merged (format "[%c-%c]" from to))
  (cl-adjoin (format "[%c-%c]" from to) merged :test #'equal))

(peg-add-method merge-error * (merged exp)
  (peg-merge-error exp merged))

(peg-add-method merge-error any (merged)
  ;; (add-to-list 'merged '(any))
  (cl-adjoin '(any) merged :test #'equal))

(peg-add-method merge-error not (merged x)
  ;; (add-to-list 'merged `(not ,x))
  (cl-adjoin `(not ,x) merged :test #'equal))

(peg-add-method merge-error action (merged _) merged)
(peg-add-method merge-error null (merged) merged)

;;; Tests:

(defmacro peg-parse-string (rules string &optional noerror)
  "Parse STRING according to RULES.
If NOERROR is non-nil, push nil resp. t if the parse failed
resp. succeded instead of signaling an error."
  `(with-temp-buffer
     (insert ,string)
     (goto-char (point-min))
     ,(if noerror
	  (let ((entry (make-symbol "entry"))
		(start (caar rules)))
	    `(peg-parse (,entry (or (and ,start `(-- t)) ""))
			. ,rules))
	`(peg-parse . ,rules))))

(provide 'peg)

;;; peg.el ends here
