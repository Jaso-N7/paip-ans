(in-package :cl-user)

;; 3.1
((lambda (x)
   ((lambda (y) (+ x y))
    (* x x)))
 6)

;; 3.2 LIST* ?

;; 3.3

(defun prind (a-list)
  "Prints an expression in dotted pair notation."
  (princ "(")
  (labels
      ((rec (element res)
	 (cond ((endp res)
		(princ element))
	       ((null (cdr res))
		(princ element)
		(princ " . ")
		(rec (first res) (cdr res)))
	       (t (princ element)
		  (princ " ")
		  (rec (first res) (cdr res))))))
    (rec (first a-list) (rest a-list)))
  (princ ")")
  a-list)

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
  )

(format t "~&Tests be run anytime with~%
(c3-examples) ; All Unit Tests~%
(c3-props)    ; Test all properties~%
or test everything~%
(progn (c3-examples) (c3-props))")
