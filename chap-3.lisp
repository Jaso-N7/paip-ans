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

;;; TESTS

(unless (find-package :ptester)
  (ql:quickload :ptester))
(use-package :ptester)

(defun c3-examples ()
  "Chapter 3 Unit tests."
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
  )

(format t "~&Tests be run anytime with~%
(c3-examples) ; All Unit Tests~%
(c3-props)    ; Test all properties~%
or test everything~%
(progn (c3-examples) (c3-props))")
