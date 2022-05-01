(defpackage chap-1
  (:documentation "1.11 Exercises Usage: Load this file then '(in-package :chap-1)'")
  (:use :cl)
  (:export #:last-name
	   #:power))

(in-package :chap-1)

;;; DATA DEFINITIONS

(defparameter *suffixes*
  '(MD. MD JR. JR SR. SR G.P. GP. GP PhD)
  "Types of Suffixes that may appear at the end of persons names")

;;; FUNCTION DEFINITIONS 

(defun last-name (name)
  "Returns a surname, ignoring titles"
  (if (member (car (last name)) *suffixes*)
      (last-name (butlast name))
      (car (last name))))

(defun power (x y)
  "Write a function to exponentiate, or raise a number X to an integer power Y.
For example: (power 3 2) = 3^2 = 9"
  0)
