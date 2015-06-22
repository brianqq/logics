;;;; first-order-res.lisp
(defpackage #:first-order-res
  (:use #:cl #:optima #:iterate))
(in-package #:first-order-res)

;;; "first-order-res" goes here. Hacks and glory await!

(defmacro aif (test truefm &rest falsefm)
  `(let ((it ,test))
     (if it ,truefm ,falsefm)))
;;; a macro is a code transform
;;; since your code is in tree structure, you can perform tree transforms on it
;;; macros let you define these
;;; they're bascially functions called at compile time. They eat code and spit out code.
;;; if you say
#|
(aif (+ 1 1)
     (+ it 3)
     (- it 2))
|#
;;; the macro call makes the substitutions
;;; test => (+ 1 1), truefm => (+ it 3), falsefm => (- it 2)
;;; then this generates the code
#|
(let ((it (+ 1 1)))
  (if it (+ it 3) (- it 2)))
|#
;;; => (+ 2 3) =>  5
;;; this macro lets you reference your test case a la perl's variables $_ 
#|
(defun flatten (L)
"Converts a list to single level."
    (if (null L)
        nil
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))
|#
;;; stole this from stack overflow

(defparameter *ops* '(:and :or :not :if :iff :forall :exists :pred :skol)
  "These are the logical operators that have special meanings`")

(defun op? (x)
  (member x *ops*))

(defun proc-pattern (ptrn)
  (if (listp ptrn) (cons 'list (mapcar #'proc-pattern ptrn)) 
      ptrn))

(defun proc-skel (skel)
  (match skel
    ((guard (list* op rest) (op? op)) (cons 'list (cons op (proc-skel rest))))
    ((type list) (mapcar #'proc-skel skel))
    ((satisfies atom) skel)))

(defmacro def-trans (name docstring &body rules)
  "a domain specific language for defining pattern-matching transformations"
  (let ((sentence (gensym "sentence")))
    `(defun ,name (,sentence) 
       ,docstring
       (match ,sentence
	 ,@(iter (for (ptrn skel) in rules)
		 (collect
		     (let ((variables
			    (remove '_
				    (remove-if #'op?
					       (remove-if-not #'symbolp
							      (flatten ptrn))))))
		      (list (proc-pattern ptrn)
			    `(let (,@(iter (for var in variables) 
					   (collect (list var (list name var)))))
					;rebind to implement recursion
			       ,(proc-skel skel))))))
	 ((satisfies atom) ,sentence) ;base case
	 ((type list) (mapcar #',name ,sentence))))))

(defmacro def-trans-comp (name docstring &body parts)
  "Defines a composition of transformers"
  (let ((sentence (gensym "sentence")))
    `(defun ,name (,sentence)
       ,docstring
       ,(reduce (lambda (queue next)
		  (cons next (list queue)))
		parts
		:initial-value sentence))))

(def-trans simplify
  "some basic boolean algebra simplifications"
  ((:not (:not x)) x)
  ((:and 0 _) 0)
  ((:and _ 0) 0)
  ((:or 0 x) x)
  ((:or x 0) x)
  ((:or _ 1) 1)
  ((:or 1 _) 1))
;;; the colon prefix makes a keyword.
;;; this is the same as a symbol, except keywords are self-evaluating
;;; so > :not => :not, whereas > 'not => not. I was getting a headache
;;; requoting everything each time it's processed.

(def-trans expand-if
  "expands if and iff into equivalent statements using and, or, not"
  ((:if a b) (:or (:not a) b))
  ((:iff a b) (:and (:or (:not a) b) (:or a (:not b)))))
;; to inspect a macro expansion, type (pprint (macroexpand-1 '(def-trans de-morgan ''blah'' ((:not (:or a b)) (:and (:not a) (:not b))))))))) 

(def-trans de-morgan
  "uses de morgan's law as a step in converting to negative normal form"
  ((:not (:or a b)) (:and (:not a) (:not b)))
  ((:not (:and a b)) (:or (:not a) (:not b)))) 

(def-trans-comp neg-norm-form
  "converts to negative normal form"
  expand-if
  de-morgan)

(defun subs (sentence var val)
  "makes a variable substitution"
  ;should this eventually handle skolem function + predicate substitutions?
  (match sentence
    ((list :skolem fn subsent) (list :skolem fn (substitute subsent var val))) 
    ((list :pred fn subsent) (list :pred fn (substitute subsent var val)))
    ((list* things) (mapcar (lambda (x) (subs x var val)) things))
    ((guard thing (equal thing var)) val)
    (_ sentence)))

(def-trans unique-binds
  "replaces bound variables with unique names"
  ((:forall x stuff) (subs (:forall x stuff) x (gensym (string x)))))
;;; gensym might break things. If it does, then I'll roll my own.
;;; But as long as we don't re-intern the gensym, it should work fine.

(def-trans skolemize-1
  "moves quantifiers to the left"
  ((:and a (:forall b stuff)) (:forall b (and a stuff)))
  ((:or a (:forall b stuff)) (:forall b (or a stuff)))
  ((:and a (:exists b stuff)) (:exists b (and a stuff)))
  ((:or a (:exists b stuff)) (:exists b (or a stuff))))


