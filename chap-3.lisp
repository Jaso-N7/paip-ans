(in-package :cl-user)

;; 3.1
((lambda (x)
   ((lambda (y) (+ x y))
    (* x x)))
 6)

;; 3.2 LIST*

;; 3.3

(defun prind (list)
  "Prints an expression in dotted-pair notation."
  (labels
      ((prind-rec (el list n-paren)
	 (cond ((null el)
		(princ nil)
		(close-parens n-paren))
	       ((atom el)
		(princ-atom el)
		(prind-rec (first list) (rest list) (1+ n-paren)))
	       (t (prind-rec (first list) (rest list) (1+ n-paren))))))
    (prind-rec (first list) (rest list) 0))
  list)

;; 3.4

(defun dottedp (list)
  "Checks whether LIST is a Dotted List. Returns T if it is; Otherwise NIL"
  (if (null (cdr (last list)))
      nil
      t))

(defun printd (list)
  "Prints an expression in dotted pair notation when necessary; Normal list
notation otherwise."
  (if (dottedp list)
      (printd-aux (first list) (rest list) 0)
      (prind list))
  list)

(defun printd-aux (atom list parens)
  "Auxilliary function for printing dotted lists in dotted-pair notation."
  (cond ((and (atom atom))
	 (princ atom)
	 (close-parens parens))
	((atom atom)
	 (princ-atom atom)
	 (printd-aux (first list) (rest list) (1+ parens)))
	(t (printd-aux (first list) (rest list) (1+ parens)))))

(defun princ-atom (atom)
  "Prints the first element in a cons."
  (princ "(") (princ atom) (princ " . "))

(defun close-parens (parens)
  "Prints the required amount of closing parenthesis."
  (dotimes (i parens)
    (princ ")")))

;; 3.5

;; Ref: https://en.wikipedia.org/wiki/Twenty_questions
;; Any type of thing.
;; Data structure: Binary Search Tree? Hash Maps?
;; Should the Data structure be initialized, or allowed to grow from nothing?

;; 3.6
;; Without testing the code, this is my response.
;; (local-a local-b global-b local-a local-b) ?

;; 3.9
(defun breadth (sequence)
  "Returns the number of elements in a sequence"
  (if (null sequence)
      0
      (let ((counter 1))
	(reduce #'(lambda (first second)
		    (cond (first
			   (incf counter 1))
			  (second
			   (incf counter 1))))
		sequence)
	counter)))

;; 3.10
;; LCM :- returns the least common multiple of the integers.
;; NRECONC :- reverses the order of elements in list (as if by nreverse).
;;            It then appends (as if by nconc) the tail to that reversed 
;; list and returns the result.

;; 3.11
;; ACONS

;;; ----------------------------------------------------------------------------

;;; TESTS

(unless (find-package :ptester)
  (ql:quickload :ptester))
(use-package :ptester)

(defun test-c3 ()
  "Chapter 3 Example / Regression tests."
  (with-tests (:name "Meaning of life...")
    (test 42 ((lambda (x)
		((lambda (y) (+ x y))
		 (* x x)))
	      6)))
  (terpri)
  (with-tests (:name "Prints Dotted-Pair notation")
    (let ((sample '(a b c d)))
      ;; (A . (B . (C . (D . NIL))))
      (test (princ sample) (prind sample))
      ;; (0 . (A . (B . (C . (D . NIL)))))
      (test (princ (cons 0 sample)) 
	    (prind (cons 0 sample))
	    :test #'equal)
      ;; (A . (B . (C . (D . (1 . (2 . (3 . NIL)))))))
      (test (princ (append sample '(1 2 3))) 
	    (prind (append sample '(1 2 3)))
	    :test #'equal))
    (let ((nested '((1 2) 3)))
      (test (princ nested) (prind nested)))
    )
  (terpri)
  (with-tests (:name "Checks for Dotted Lists")
    (test t (dottedp (cons 'a 'b)))
    (test t (dottedp (cons 'a (cons 'b 'c))))
    (test nil (dottedp (cons 'a nil)))
    (test nil (dottedp (cons 'a (cons 'b nil))))
    (test t (dottedp '((1 2) . 3)))
    (test nil (dottedp '((1 2) 3))))
  (terpri)
  (with-tests (:name "A version of LENGTH using REDUCE")
    (test 0 (breadth nil))
    (test 0 (breadth '()))
    (test 1 (breadth '((#\c))))
    (test 3 (breadth '(a (b) 2)))
    (test 5 (breadth '(1 2 5 4 3)))
    (test 5 (breadth (vector 'a 'b 'c 'd 'e)))
    (test 4 (breadth "paip")))
  )

(format t "~&Tests be run anytime with~%
(test-c3) ; Regression Tests")
