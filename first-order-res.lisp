;;;; first-order-res.lisp
(defpackage #:first-order-res
  (:use #:cl #:optima #:iterate))
(in-package #:first-order-res)

;;; "first-order-res" goes here. Hacks and glory await!

(defmacro aif2 (test truefm falsefm)
  (let ((res (gensym "res")))
    `(multiple-value-bind (it ,res) ,test
	 (if ,res ,truefm ,falsefm))))

(defun flatten (L)
"Converts a list to single level."
    (if (null L)
        nil
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))

(defparameter *ops* '(:and :or :not :if :iff :forall :exists :pred :skol))

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
		 (collect (list (proc-pattern ptrn)
				`(let (,@(iter (for var in (remove-if #'op? (flatten ptrn)))
					       (collect (list var (list name var)))))
				   ,(proc-skel skel)))))
	 (_ ,sentence)))))

(defmacro def-trans-comp (name docstring &body parts)
  "Defines a composition of transformers"
  (let ((sentence (gensym "sentence")))
    `(defun ,name (,sentence)
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
;;; equivalently, this could be defined as the function 
#|
(defun expand-if (sentence)
  "expands if and iff into equivalent statements using and, or, not" 
  (match sentence
    ((list :if a b) `(:or (:not ,(expand-if a) ,(expand-if b))))
    ((list :iff a b) `(:and (:or (:not ,(expand-if a)) ,(expand-if b))
			    (:or ,(expand-if a) (:not ,(expand-if b)))))))
|#

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
    ((list 'skolem fn subsent) (list 'skolem fn (substitute subsent var val))) 
    ((list 'pred fn subsent) (list 'pred fn (substitute subsent var val)))
    ((list* things) (mapcar (lambda (x) (subs x var val)) things))
    ((guard thing (equal thing var)) val)
    (_ sentence)))

(def-trans unique-binds
  "replaces bound variables with unique names"
  ((:forall x stuff) (subs (:forall x stuff) x (gensym (string x)))))
;;; gensym might break things. If it does, then I'll roll my own.

(def-trans skolemize-1
  ((:and a (:forall b stuff)) (:forall b (and a stuff)))
  ((:or a (:forall b stuff)) (:forall b (or a stuff)))
  ((:and a (:exists b stuff)) (:exists b (and a stuff)))
  ((:or a (:exists b stuff)) (:exists b (or a stuff))))

(defun conj-norm-form (sentence)
)
