;; Solutions for Chapter 1 exercises.
;; 
;; This file is meant to be loaded or compiled \(or both).
;; As packages were not introduced, I decided to keep it simple and stand-alone.
;;
;; Tests are at the bottom of this file, or they can be run anytime with
;;
;; > (c1-examples)
;; > (c1-pbts)
;; > (progn (c1-examples) (c1-pbts))
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

(defun power (base n)
  "Write a function to exponentiate, B raised to the power of N.
For example: (power 3 2) = 3^2 = 9"
  (declare (notinline power))
  (let ((b (abs base)))
    (cond ((= n 1) b)
	  ((zerop n) 1)
	  ((and (minusp n) (>= b 0))
	   (/ 1 (power b (abs n))))
	  (t  (* b (power b (1- n)))))))

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
  (terpri)
  (format t "~&>>> REGRESSION & EXAMPLE TESTING... <<<~%")
  (terpri)
  (with-tests (:name "Unit 1.1: LAST-NAME")
    (test 'MORGAN (last-name '(rex morgan md)))
    (test 'DOWNEY (last-name '(morton downey jr.)))
    (test 'FLYNN (last-name '(Flynn PhD)))
    (test 'WILSON (last-name '(Tom Wilson))))
  (terpri)

  (with-tests (:name "Unit 1.2: POWER")
    (test 9 (power 3 2))
    (test 1 (power 3 0))
    (test 1 (power 0 0))
    (test-error (power 0 -1) :condition-type 'condition
			     :include-subtypes t)
    (test 1/9 (power 3 -2)))
  (terpri)

  (with-tests (:name "Unit 1.3: COUNT-ATOMS")
    (test 0 (count-atoms '()))
    (test 1 (count-atoms 'a))
    (test 5 (count-atoms '(a b c #\d "e")))
    (test 3 (count-atoms '((a) b c)))
    (test 3 (count-atoms '(a (b) c)))
    (test 3 (count-atoms '(a b (c))))
    (test 3 (count-atoms '(() b c)))
    (test 3 (count-atoms '(a () c)))
    (test 3 (count-atoms '(a b ()))))
  (terpri)

  (with-tests (:name "Unit 1.4: COUNT-ANYWHERE")
    (test 0 (count-anywhere 'a '(c (c b) d)))
    (test 0 (count-anywhere 'a '()))
    (test 0 (count-anywhere 'a '(1 2 3)))
    (test 1 (count-anywhere 'a '(a 1 2 3)))
    (test 1 (count-anywhere 'a '(1 a 3)))
    (test 1 (count-anywhere 'a '(1 2 3 a)))
    (test 2 (count-anywhere 'a '(a b b a)))
    (test 3 (count-anywhere 'a '(a ((a) b) a)))
    (test 3 (count-atoms '(lambda () #'identity)))
    (test 1 (count-atoms #'identity)))
  (terpri)

  (with-tests (:name "Unit 1.5: DOT-PRODUCT")
    (test 110 (dot-product '(10 20) '(3 4))))
  (terpri))

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

(defparameter *elements*
  (append '(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 a b c d e f g)
	  '(#\a #\b #\c #\d #\e #\f #\g)
	  '("h" "i" "j" "k" "l" "m" "n" "o")
	  '(common lisp is such pureness)
	  '(1e-2 3e-4 5e-6 7e-8)
	  '($ ~ % { } [ nil ] ! + * ))
  "Mainly used for the generating of mixed lists")

;;; GENERATORS

;; Used to generate a name with or without titles / suffixes.
;;   "Randomly generates a valid name"
;; Test examples:
;; > (generate (generator (latin-name *namedata*)))
;; > (BRIAN BRIAN G.P.) ; May or may not have a title
;; or
;; > (generate (generator (latin-name *namedata* nil)))
;; > (ADRIAN) ; Will never have a title
(def-generator latin-name (sample &optional (suffixp (one-of t nil)))
  (generator (map (lambda (names add-suffix-p)
		    (let ((limit (random (length names)))
			  (new-name))
		      (dotimes (i limit)
			(push (nth (random limit) names) new-name))
		      (if add-suffix-p
			  (add-title new-name)
			  new-name)))
		  sample
		  suffixp)))

;; Used for generating a list containing mixed symbols such as
;; > (generate (generator (mixed *elements*)))
;; > (7.0E-8 } "l" 2 7 1 5 "m" $ "i" B -1 5 5.0E-6 -6 5 5.0E-6 { 4 5 8 -6 1 6 -3
;; 7.0E-8 #\b G D -9 F #\c D % 9 -1 3 COMMON #\f "j" COMMON 1 0 #\d 5.0E-6 7 #\g
;; #\a -5 -2 "h" F "k" #\a -8)
(def-generator mixed (seed)
  (generator (map (lambda (s)
		    (let ((lim (random (length s)))
			  (lst))
		      (dotimes (i lim lst)
			(push (nth (random lim) s) lst))))
		  seed)))

;;; GENERATORS --- Helpers

(defun add-title (name)
  (let ((limit (length (get-suffixes))))
    (append name (list (nth (random limit) (get-suffixes))))))



;;; PROPERTIES

(defun c1-pbts ()
  (terpri)
  (format t "~&>>> TESTING PROPERTIES... <<<~%")
  (terpri)
  ;; -- Modeling -- Test the real implementation against an indirect
  ;; (and simple) implementation.
  (with-tests (:name "LAST-NAME Gets the last name unconditionally.")
    ;; Generates a list representing a full name, with or without a suffix
    (check-it (generator (latin-name *namedata*))
	      (lambda (fullname)
		(test (last-name fullname)
		      (funcall #'(lambda (n)
				   (cadr (reverse n))
				   (if (titlep n)
				       (cadr (reverse n))
				       (car (reverse n))))
			       fullname)))))
  (terpri)

  ;; -- Generalizing -- Abstracting away Examples
  (with-tests (:name "LAST-NAME Only returns the surname.")
    (check-it (generator (tuple
			  ;; Generate a list representing a random full name
			  ;; and ensure no empty names are returned
			  (guard (lambda (n) (not (null n)))
				 (generator (latin-name *namedata* nil)))
			  ;; Pick a random name from the data as a sure name
			  (nth (random (length *namedata*)) *namedata*)))
	      (lambda (known)
		;; Compares the result of LAST-NAME to ensure it returns
		;; the known surname
		(let* ((known-surname (cdr known))
		       (known-fullname (append (car known) known-surname)))
		  (test (car known-surname) (last-name known-fullname))))))
  (terpri)

  ;; -- Invariants --
  (with-tests (:name "LAST-NAME A person should not have more than one surname")
    (check-it (generator (latin-name *namedata*))
	      (lambda (fullname)
		(test 1 (length (list (last-name fullname)))))))
  (terpri)

  ;; ??? How to test that LAST-NAME does not add new elements / information?

  ;; ??? How to test that LAST-NAME is not destructive / removes the last name

  ;; -- Modeling --
  (with-tests (:name "POWER Calculate positive base raised to the power of n")
    (check-it (generator (tuple (integer 0) (integer)))
	      (lambda (a-tuple)
		(let ((b (car a-tuple))
		      (e (cadr a-tuple)))
		  (if (and (zerop b) (minusp e))
		      (test-error (power b e)
				  :condition-type 'condition
				  :include-subtypes t)
		      (test (power b e)
			    (expt b e)))))))
  (terpri)

  ;; -- Invariance --
  (with-tests (:name "POWER only returns either an INTEGER or a REAL")
    (let ((*num-trials* 100))
      (check-it (generator (tuple (integer 0) (integer)))
		(lambda (a-tuple)
		  (let ((b (car a-tuple))
			(e (cadr a-tuple)))
		    (if (and (zerop b) (minusp e))
			(test-error (power b e)
				    :condition-type 'condition
				    :include-subtypes t)
			(let ((ans (power b e)))
			  (or (integerp ans)
			      (realp ans)))))))))
  (terpri)

  ;; -- Identities --
  (with-tests (:name "POWER Identity B^m+n = B^m . B^n , provided base is non-zero")
    (check-it (generator (tuple
			  ;; Only provide non-zero values for the base
			  (guard (lambda (i) (or (plusp i) (minusp i))) (integer))
			  ;; Generate M and N values
			  (list (integer) :length 2)))
	      (lambda (indices)
		(let ((b (first indices))
		      (m (first (second indices)))
		      (n (second (second indices))))
		  (test (power b (+ m n))
			(* (power b m) (power b n)))))))
  (terpri)
  (with-tests (:name "POWER Identity (B^m)^n = B^m.n , provided base is non-zero")
    (check-it (generator (tuple
			  ;; Only provide non-zero values for the base
			  (guard (lambda (i) (or (plusp i) (minusp i))) (integer))
			  ;; Generate M and N values
			  (list (integer) :length 2)))
	      (lambda (indices)
		(let ((b (first indices))
		      (m (first (second indices)))
		      (n (second (second indices))))
		  (test (power (power b m) n)
			(power b (* m n)))))))
  (terpri)
  (with-tests (:name "POWER Identity (B.C)^n = B^n.C^n , provided base is non-zero")
    (check-it (generator (tuple
			  ;; Only provide non-zero values for the bases
			  (guard (lambda (i) (or (plusp i) (minusp i))) (integer))
			  (guard (lambda (i) (or (plusp i) (minusp i))) (integer))
			  ;; Generate N value
			  (integer)))
	      (lambda (indices)
		(let ((b (first indices))
		      (c (second indices))
		      (n (third indices)))
		  (test (power (* b c) n)
			(* (power b n) (power c n)))))))
  (terpri)

  ;; -- Modeling --

  (with-tests (:name "COUNT-ATOMS returns the number of atoms in an expression")
    (check-it (generator (mixed *elements*))
	      (lambda (x)
		(test (length x)
		      (count-atoms x)))))
  (terpri)

  ;; -- Generalize --
  (with-tests (:name "COUNT-ATOMS always returns 1 for atomic expressions")
    (check-it (generator (or (list (integer) :length 1)
			     (integer)
			     (character)
			     (string)))
	      (lambda (x)
		(test 1 (count-atoms x)))))
  (terpri)

  ;; -- Commutative --
  (with-tests (:name "COUNT-ATOMS Commutative a + b = b + a")
    (check-it (generator (tuple (mixed *elements*)
				(mixed *namedata*)))
	      (lambda (xs)
		(let ((a (first xs))
		      (b (second xs)))
		  (test (count-atoms (append a b))
			(count-atoms (append b a)))))))
  (terpri)

  

  )

;;; HELPERS

(defun titlep (name)
  "Confirms if the name is suffixed with a title.
Returns T if it does; Otherwise NIL"
  (member (car (reverse name)) (get-suffixes)))

;;; RUN

(c1-examples)
(c1-pbts)
