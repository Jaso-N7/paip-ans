(defpackage paip-ans/models
  (:documentation "Data models supporting solutions")
  (:use :cl)
  (:export #:get-suffixes
	   #:set-suffix))

(in-package :paip-ans/models)

(defparameter *suffixes*
  '(MD. MD JR. JR SR. SR G.P. GP. GP PhD)
  "Types of Suffixes that may appear at the end of persons names")

(defun get-suffixes ()
  "Returns the suffixes"
  *suffixes*)

(defun set-suffix (new-title)
  "Adds another suffix"
  (pushnew new-title *suffixes*))
