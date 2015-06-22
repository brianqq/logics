;;;; first-order-res.lisp
(defpackage #:first-order-res
  (:use #:cl #:optima #:iterate))
(in-package #:first-order-res)

;;; "first-order-res" goes here. Hacks and glory await!

(defun neg-norm-form (sentence)
  (match sentence
    ((list 'if a b) `(or (not ,(neg-norm-form a)) ,(neg-norm-form b)))
    ((list 'iff a b) `(and
		       (or (not ,(neg-norm-form a)) ,(neg-norm-form b))
		       (or ,(neg-norm-form a) (not ,(neg-norm-form b)))))
    ((list* car cdr) (cons (neg-norm-form car)
			   (neg-norm-form cdr)))
    (_ sentence)))

(defun conj-norm-form (sentence)
  wh)
