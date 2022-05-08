(in-package :cl-user)

;; 3.1
((lambda (x)
   ((lambda (y) (+ x y))
    (* x x)))
 6)

;; 3.2 LIST* ?

;; 3.3

((lambda (lis)
   (labels
       ((rec (l res)
	  (cond ((endp res)
		 (princ l))
		((null (cdr res))
		 (princ l)
		 (princ ".")
		 (rec (first res) (cdr res)))
		(t (princ l)
		   (rec (first res) (cdr res))))))
     (rec (first lis) (rest lis))))
 '(a b c d c e c))

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
