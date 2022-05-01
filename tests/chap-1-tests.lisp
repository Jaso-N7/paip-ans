(defpackage paip-ans/tests.chap-1
  (:documentation "Property-Based and Unit Tests for Chapter 1.11 Exercises.")
  (:use :cl :cl-quickcheck)
  (:import-from :paip-ans/tests.generators
		#:english-name)
  (:export #:c1-examples
	   #:c1-pbts))

(in-package :paip-ans/tests.chap-1)

;;; DATA DEFINITIONS


;;; UNIT / EXAMPLES

(defun c1-examples ()
  (named "Unit 1.1: Returns surname regardless if suffixed or not"
	 (is= 'MORGAN (last-name '(rex morgan md)))
	 (is= 'DOWNEY (last-name '(morton downey jr.)))
	 (is= 'FLYNN (last-name '(Flynn PhD))))
  (named "Unit 1.2: Test for exponentiation."
	 (is= 9 (power 3 2)))

  (named "Unit 1.3: Counting number of atoms in an expression."
	 (is= 0 (count-atoms '()))
	 (is= 1 (count-atoms 'a))
	 (is= 3 (count-atoms '(a (b) c)))
	 (is= 3 (count-atoms '(a () c))))

  (named "Unit 1.4: Counting the number of times an expression
occurs anywhere within another expression."
	 (is= 0 (count-anywhere 'a '(c (c b) d)))
	 (is= 0 (count-anywhere 'a '()))
	 (is= 0 (count-anywhere 'a '(1 2 3)))
	 (is= 1 (count-anywhere 'a '(a 1 2 3)))
	 (is= 1 (count-anywhere 'a '(1 a 3)))
	 (is= 1 (count-anywhere 'a '(1 2 3 a)))
	 (is= 2 (count-anywhere 'a '(a b b a)))
	 (is= 3 (count-anywhere 'a '(a ((a) b) a)))))

;;; PROPERTIES

(defun c1-pbts ()
  nil)

;;; HELPERS


