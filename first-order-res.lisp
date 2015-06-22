;;;; first-order-res.lisp
(defpackage #:first-order-res
  (:use #:cl #:optima #:iterate))
(in-package #:first-order-res)

;;; "first-order-res" goes here. Hacks and glory await!

(defun neg-norm-form (sentence)
  (match sentence
    ((list ('if a b)) '(or (not a) b))))

(defun conj-norm-form (sentence)
  wh)
