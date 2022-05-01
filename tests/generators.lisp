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

(defparameter *textdata*
  (concatenate 'string
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
	       "-'")
  "Contains all the valid characters that makes up (an anglican?) name")

;;; GENERATORS

(define (english-name)
  "Used to generate a name without suffixes"
  (generate (a-name *textdata*)))

;; Should I use STRING-CAPITALIZE, or prepend a capital letter?
(defun a-name (sample-text)
  (lambda ()
    (let* ((limit (length sample-text))
	   (to-string (make-string (random limit)
				   :initial-element #\SPACE))
	   (string-lim (length to-string)))
      (dotimes (i string-lim to-string)
	(setf (char to-string i)
	      (char sample-text (random limit)))))))
