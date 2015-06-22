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

(defmacro def-trans (name &body rules)
  "A more concise way to define a pattern-matching transformatioen"
  (let ((sentence (gensym "sentence")))
    `(defun ,name (,sentence) 
       (match ,sentence
	 ,@(iter (for (ptrn skel) in rules)
		 (collect (list (proc-pattern ptrn)
				`(let (,@(iter (for var in (remove-if #'op? (flatten ptrn)))
					       (collect (list var (list name var)))))
				   ,(proc-skel skel)))))
	 (_ ,sentence)))))

(defmacro def-trans-comp (name &body parts)
  (let ((sentence (gensym "sentence"))
	(names (mapcar #'car parts)))
    `(progn
       ,@(iter (for part in parts)
	       (collect (cons 'def-trans part)))
      (defun ,name (,sentence)
	,(reduce (lambda (queue next)
		   (cons next (list queue)))
		 names
		 :initial-value sentence)))))

(def-trans simplify
  ((:not (:not x)) x)
  ((:and 0 _) 0)
  ((:and _ 0) 0)
  ((:or 0 x) x)
  ((:or x 0) x)
  ((:or _ 1) 1)
  ((:or 1 _) 1))

(def-trans-comp neg-norm-form
 (expand-if
   ((:if a b) (:or (:not a) b))
   ((:iff a b) (:and (:or (:not a) b) (:or a (:not b)))))
  (de-morgan
   ((:not (:or a b)) (:and (:not a) (:not b)))
   ((:not (:and a b)) (:or (:not a) (:not b)))))

(defun subs (sentence var val)
  ;should this eventually handle skolem function + predicate substitutions?
  (match sentence
    ((list 'skolem fn subsent) (list 'skolem fn (substitute subsent var val))) 
    ((list 'pred fn subsent) (list 'pred fn (substitute subsent var val)))
    ((list* things) (mapcar (lambda (x) (subs x var val)) things))
    ((guard thing (equal thing var)) val)
    (_ sentence)))

(def-trans unique-binds
  ((:forall x stuff) (subs (:forall x stuff) x (gensym (string x)))))

(def-trans skolemize-1
  ((and a (:forall b stuff)) (:forall b (and a stuff)))
  ((or a (:forall b stuff)) (:forall b (or a stuff)))
  ((and a (:exists b stuff)) (:exists b (and a stuff)))
  ((or a (:exists b stuff)) (:exists b (or a stuff))))

(defun conj-norm-form (sentence)
)
