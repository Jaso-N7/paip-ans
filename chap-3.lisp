(in-package :cl-user)

;; 3.1
((lambda (x)
   ((lambda (y) (+ x y))
    (* x x)))
 6)

;; 3.2 LIST* ?

;; 3.3
;; "Prints an expression in dotted-pair notation."
((lambda (a-list)
	    (labels
		((rec (el list n-paren)
		   (cond ((null el)
		       (princ nil)
		       (close-parens n-paren))
		      ((atom el)
		       (print-atom el)
		       (rec (first list) (rest list) (1+ n-paren)))
		      (t (rec (first list) (rest list) (1+ n-paren)))))
		 (print-atom (atm)
		   (princ "(")
		   (princ atm)
		   (princ " . "))
		 (close-parens (parens)
		   (dotimes (i parens)
		     (princ ")"))))
	      (rec (first a-list) (rest a-list) 0))
	    a-list)
	  '(a b c d))

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
      (prind list)
      (progn
	(princ "(")
	(dolist (l list (princ ")"))
	  (princ l)
	  (princ " "))))
  list)

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
  (with-tests (:name "Checks for Dotted Lists")
    (test t (dottedp (cons 'a 'b)))
    (test t (dottedp (cons 'a (cons 'b 'c))))
    (test nil (dottedp (cons 'a nil)))
    (test nil (dottedp (cons 'a (cons 'b nil)))))
  )

(format t "~&Tests be run anytime with~%
(c3-examples) ; All Unit Tests~%
(c3-props)    ; Test all properties~%
or test everything~%
(progn (c3-examples) (c3-props))")
