(defpackage paip-ans
  (:documentation "1.11 Exercises Usage: Load this file then '(in-package :chap-1)'")
  (:use :cl)
  (:export #:last-name
	   #:power
	   #:count-atoms
	   #:count-anywhere))

(in-package :paip-ans)

;;; DATA DEFINITIONS

(defparameter *suffixes*
  '(MD. MD JR. JR SR. SR G.P. GP. GP PhD)
  "Types of Suffixes that may appear at the end of persons names")

;;; FUNCTION DEFINITIONS 

(defun last-name (name)
  "Returns a surname, ignoring titles"
  (if (member (car (last name)) *suffixes*)
      (last-name (butlast name))
      (car (last name))))

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
