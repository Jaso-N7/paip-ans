(defpackage paip-ans/tests.generators
  (:documentation "Provide custom generators for the properties")
  (:use :cl :cl-quickcheck)
  (:export #:english-name))

(in-package :paip-ans/tests.generators)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro one-of (&rest exprs)
    "Courtesy of Paul Graham's ANSI CL - Macros pg 170."
    `(case (random ,(length exprs))
       ,@(let ((key -1))
	   (mapcar #'(lambda (expr)
		       `(,(incf key) ,expr))
		   exprs)))))

;;; DATA DEFINITIONS

(defparameter *namedata*
  '(adrian brian charlie dean erik)
  "Symbols to represent a name")

;;; GENERATORS

(define (english-name)
  "Used to generate a name without suffixes"
  (generate (a-name *namedata*)))

;; Should I use STRING-CAPITALIZE, or prepend a capital letter?
(defun a-name (sample-symbols)
  (lambda ()
    (let ((limit (random (length *namedata*)))
	  (new-name))
      (dotimes (i limit new-name)
	(push (nth i *namedata*) new-name)))))
