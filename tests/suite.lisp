(defpackage paip-ans/tests.suite
  (:documentation "Property-Based Tests for PAIP.")
  (:use :cl :paip-ans :cl-quickcheck)
  (:import-from :paip-ans/tests.chap-1
		#:c1-examples
		#:c1-pbts)
  (:export #:run))

(in-package :paip-ans/tests.suite)

(defparameter *skipped*
  (vector #'c1-pbts)
  "Do not run these tests.")

(defparameter *suite*
  (vector #'c1-examples
	  #'c1-pbts)
  "Test Suite. Holds the names of all the tests to be run.")

(defun run ()
  "Run ALL tests."
  (dotimes (index (length *suite*))
    (let ((test-name (svref *suite* index)))
      (unless (find test-name *skipped*)
	(funcall test-name)))))
