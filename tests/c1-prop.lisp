(defpackage paip-ans/tests.property
  (:documentation "Property-Based and Unit Tests for Chapter 1.11 Exercises.")
  (:use :cl :cl-quickcheck :paip-ans)
  (:import-from :paip-ans/tests.generators
		#:english-name)
  (:import-from :paip-ans/models
		#:get-suffix)
  (:export #:c1-pbts))

(in-package :paip-ans/tests.property)

;;; DATA DEFINITIONS

(defparameter #'someone
  (lambda ()
    (pick-weighted
      (1 (a-list english-name))
      (2 (append (a-list english-name)
		 (nth (random (length (get-suffix)))
		      (get-suffix))))))
  "Randomly generate a name with or without a title")

;;; PROPERTIES

(defun c1-pbts ()
  (named "PBT 1.1 Finds the surname regardless of title."
    (for-all ((name #'someone))
      )))

;;; HELPERS

