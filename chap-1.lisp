;; Solutions for Chapter 1 exercises.
;; 
;; This file is meant to be loaded or compiled \(or both).
;; As packages were not introduced, I decided to keep it simple and stand-alone.
;;
;; Tests are at the bottom of this file.
;; 
;; 

(in-package :cl-user)

;;; DATA DEFINITIONS

(defparameter *suffixes*
  '(MD. MD JR. JR SR. SR G.P. GP. GP PhD ESQ)
  "Titles that may appear at the end of names.")

(defun get-suffixes ()
  "Query the suffixes."
  *suffixes*)

(defun set-suffix (new-title)
  "Command to add another suffix."
  (pushnew new-title *suffixes*))

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


;;; SELF-EXAMPLES

(progn  (ql:quickload :ptester)
	(use-package :ptester))

;;; TESTS

;; > (ptester:with-tests (:name "Grok")
;; 	   (ptester:test 1 1))
(defun c1-examples ()
  "Example-Based testing for section 1.11"
  (with-tests (:name "Unit 1.1: Returns surname regardless if suffixed or not")
    (test 'MORGAN (last-name '(rex morgan md)))
    (test 'DOWNEY (last-name '(morton downey jr.)))
    (test 'FLYNN (last-name '(Flynn PhD)))
    (test 'WILSON (last-name '(Tom Wilson))))

  (with-tests (:name "Unit 1.2: Test for exponentiation.")
    (test 9 (power 3 2)))

  (with-tests (:name "Unit 1.3: Counting number of atoms in an expression.")
    (test 0 (count-atoms '()))
    (test 1 (count-atoms 'a))
    (test 3 (count-atoms '(a (b) c)))
    (test 3 (count-atoms '(a () c))))

  (with-tests (:name "Unit 1.4: Counting the number of times an expression
occurs anywhere within another expression.")
    (test 0 (count-anywhere 'a '(c (c b) d)))
    (test 0 (count-anywhere 'a '()))
    (test 0 (count-anywhere 'a '(1 2 3)))
    (test 1 (count-anywhere 'a '(a 1 2 3)))
    (test 1 (count-anywhere 'a '(1 a 3)))
    (test 1 (count-anywhere 'a '(1 2 3 a)))
    (test 2 (count-anywhere 'a '(a b b a)))
    (test 3 (count-anywhere 'a '(a ((a) b) a))))

  (with-tests (:name "Unit 1.5: Dot product calculations are accurate")
    (test 110 (dot-product '(10 20) '(3 4)))))

;; -----------------------------------------------------------------------------

(progn  (ql:quickload :check-it)
	(use-package :check-it))

;;; DATA DEFINITIONS

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro one-of (&rest exprs)
    "Courtesy of Paul Graham's ANSI CL - Macros pg 170."
    `(case (random ,(length exprs))
       ,@(let ((key -1))
	   (mapcar #'(lambda (expr)
		       `(,(incf key) ,expr))
		   exprs)))))

(defparameter *namedata*
  '(adrian brian charlie derek erik frederick)
  "Symbols to represent a name")

;;; GENERATORS

;; Used to generate a name with or without titles / suffixes.
;;   "Randomly generates a valid name"
(def-generator latin-name (maker sample)
  (generator (or (funcall maker sample (one-of t nil))
		 (latin-name maker sample))))

;;; GENERATORS --- Helpers

(defun add-title (name)
  (let ((limit (length (get-suffixes))))
    (append name (list (nth (random limit) (get-suffixes))))))

;;; PROPERTIES

(defun c1-pbts ()
  
  (with-tests (:name "PBT 1.1 Gets the last name unconditionally.")
    (check-it (generator (latin-name
			  (lambda (names add-suffix-p)
			    (let ((limit (random (length names)))
				  (new-name))
			      (dotimes (i limit)
				(push (nth (random (1+ i)) names) new-name))
			      (if add-suffix-p
				  (add-title new-name)
				  new-name)))
			  *namedata*))
	      (lambda (fullname)
		(test (last-name fullname)
		      (funcall #'(lambda (n)
				   (if (titlep n)
				       (cadr (reverse n))
				       (car (reverse n))))
			       fullname))))))

;;; HELPERS

(defun titlep (name)
  "Confirms if the name is suffixed with a title.
Returns T if it does; Otherwise NIL"
  (member (car (reverse name)) (get-suffixes)))

;;; RUN

(c1-examples)
(c1-pbts)
