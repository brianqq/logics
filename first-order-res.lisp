;;;; first-order-res.lisp
(defpackage #:first-order-res
  (:use #:cl #:optima #:iterate))
(in-package #:first-order-res)

;;; "first-order-res" goes here. Hacks and glory await!

(defmacro aif2 (test truefm falsefm)
  (let ((res (gensym)))
    `(multiple-value-bind (it ,res) ,test
	 (if ,res ,truefm ,falsefm))))

(defparameter *ops* '(and or not if iff forall exists))

(defun proc-pattern (list)
  (cons 'list
	(iter (for x in list)
	      (collect
		  (cond
		    ((listp x) (proc-pattern x))
		    ((member x *ops*) `(quote ,x))
		    (t x))))))

(defun proc-skel (recur list)
  (cons 'list
	(iter (for x in list)
	      (collect (cond
			 ((listp x) (proc-skel recur x))
			 ((member x *ops*) `(quote ,x))
			 (t `(,recur ,x)))))))

(defmacro def-trans (name &body rules)
  "A more concise way to define a pattern-matching transformatioen"
  `(defun name (sentence) 
     (match sentence
       ,@(iter (for (ptrn skel) in rules)
	       (collect (list (proc-pattern ptrn) (proc-skel name skel)))))))

(def-trans neg-norm-form
  ((if a b) (or (not a) b))
  ((iff a b) (and (or (not a) b) (or a (not b)))))
;;; this should expand into the following code  
;; (defun neg-norm-form (sentence)
;;   (match sentence
;;     ((list 'if a b) `(or (not ,(neg-norm-form a)) ,(neg-norm-form b)))
;;     ((list 'iff a b) `(and
;; 		       (or (not ,(neg-norm-form a)) ,(neg-norm-form b))
;; 		       (or ,(neg-norm-form a) (not ,(neg-norm-form b)))))
;;     ((list* car cdr) (cons (neg-norm-form car)
;; 			   (neg-norm-form cdr)))
;;     (_ sentence)))

(defun conj-norm-form (sentence)
  wh)
