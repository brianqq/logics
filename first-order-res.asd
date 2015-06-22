;;;; first-order-res.asd

(asdf:defsystem #:first-order-res
  :description "Describe first-order-res here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:alexandria #:optima #:iterate #:checkl #:metabang-bind #:checkl)
  :components ((:file "first-order-res")))

