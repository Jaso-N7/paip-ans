(defpackage paip-ans/tests.unit
  (:documentation "All Unit Tests")
  (:use :cl :cl-quickcheck)
  (:export #:c1-examples))

(in-package :paip-ans/tests.unit)

;;; UNIT / EXAMPLES

(defun c1-examples ()
  (named "Unit 1.1: Returns surname regardless if suffixed or not"
    (is= 'MORGAN (last-name '(rex morgan md)))
    (is= 'DOWNEY (last-name '(morton downey jr.)))
    (is= 'FLYNN (last-name '(Flynn PhD)))
    (is= 'WILSON (last-name '(Tom Wilson))))

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
    (is= 3 (count-anywhere 'a '(a ((a) b) a))))
  (named "Unit 1.5: Dot product calculations are accurate"
    (is= 110 (dot-product '(10 20) '(3 4)))))
