(defpackage paip-ans/c1
  (:documentation "1.11 Exercises Usage: Load this file then '(in-package :chap-1)'")
  (:use :cl)
  (:import-from :paip-ans/models
		#:get-suffixes
		#:set-suffix)
  (:export #:last-name
	   #:power
	   #:count-atoms
	   #:count-anywhere
	   #:dot-product
	   *suffixes*))

(in-package :paip-ans/c1)

;;; DATA DEFINITIONS


;;; FUNCTION DEFINITIONS 

(defun last-name (name)
  "Returns a surname, ignoring titles"
  (cond ((null name) nil)
	((member (car (last name)) (get-suffixes))
	 (last-name (butlast name)))
	(t (car (last name)))))

(defun power (x y)
  "Write a function to exponentiate, or raise a number X to an integer power Y.
For example: (power 3 2) = 3^2 = 9"
  (expt x y))

(defun count-atoms (expr)
  "Counts the number of atoms in an expression."
  (cond ((null expr) 0)
	((atom expr) 1)
	(t (1+ (count-atoms (cdr expr))))))

(defun count-anywhere (item expr)
  "Counts the number of times an expression ITEM occurs anywhere within another
expression EXPR.
Example: (count-anywhere 'a '(a ((a) b) a)) => 3"
  (cond ((null expr) 0)
	((consp (car expr))
	 (+ (count-anywhere item (car expr))
	    (count-anywhere item (cdr expr))))
	((eql item (car expr))
	 (+ 1 (count-anywhere item (cdr expr))))
	(t (+ 0 (count-anywhere item (cdr expr))))))

(defun dot-product (ms ns)
  "Computes the dot product of two sequences of numbers, represented as lists.
Example: (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110"
  (apply #'+ (mapcar #'* ms ns)))
