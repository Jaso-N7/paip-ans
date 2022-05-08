;; Solutions for Chapter 2 exercises.
;; 
;; This file is meant to be loaded or compiled \(or both).
;; As packages were not introduced, I decided to keep it simple and stand-alone.
;;
;; Tests are at the bottom of this file, or they can be run anytime with
;;
;; > (c2-examples)
;; > (c2-props)
;; > (progn (c2-examples) (c2-props))
;;

(in-package :cl-user)

;;; PAIP DEFINITIONS -- Code from the book required for the exercises

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

;; (defun one-of (set)
;;   "Pick one element of set, and make a list of it."
;;   (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

;; Had to rename so it doesn't clash with CHECK-IT:GENERATE
(defun generate-phrase (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate-phrase phrase))
        ((rewrites phrase)
         (generate-phrase (random-elt (rewrites phrase))))
        (t (list phrase))))

;;; EXERCISES

;; 2.1
(defun generate-english (phrase)
  "Generates a random english sentence or phrase.
> (generate-english 'sentence) => (A MAN SAW THE MAN)
> (generate-english 'sentence) => (THE MAN TOOK A BALL)
> (generate-english 'noun-phrase) => (THE WOMAN)
> (generate-english 'verb-phrase) => (HIT A WOMAN)
> (generate-english 'word) => (WORD)"
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase)
	   (mappend #'generate-english phrase))
	  ((null choices)
	   (list phrase))
	  (t  (generate-english (random-elt choices))))))

;; 2.2 - I could not figure this one out on my own.
(defun generate-grammar (phrase)
  "Generates a random sentence or phrase while differentiating
the terminal and non-terminal."
  (cond ((listp phrase)
	 (mappend #'generate-grammar phrase))
	((non-terminal-p phrase)
	 (generate-grammar (random-elt (rewrites phrase))))
	(t  (list phrase))))

(defun non-terminal-p (category)
  "T if this is a category in the grammar."
  (not (null (rewrites category))))

;;; TESTS -- Units and Property-Based

(unless (find-package :ptester)
  (ql:quickload :ptester)
  (use-package :ptester))

(defun c2-examples ()
  "Unit tests for Chapter 2 exercises."
  (with-tests (:name "GENERATE-ENGLISH returns exact phrase as a list")
    (test '(word) (generate-english 'word) :test #'equal)
    (test nil (generate-english '()))
    (test '(the phrase) (generate-english '(the phrase)) :test #'equal))
  (terpri)
  (with-tests (:name "GENERATE-GRAMMAR returns exact phrase as a list")
    (test '(word) (generate-grammar 'word) :test #'equal)
    (test nil (generate-grammar '()))
    (test '(the phrase) (generate-grammar '(the phrase)) :test #'equal))
  (terpri))

(unless (find-package :check-it)
  (ql:quickload :check-it)
  (use-package :check-it))

;; PROPERTIES

(defun c2-props ()
  "Property-Based Tests for Chapter 2 exercises."

  ;; Invariances -- Try to test for obvious correctness

  ;; (with-tests (:name "A valid Sentence => Noun phrase + Verb phrase")
  ;;   ())

  ;; Postconditions -- What should be true after calling a function?

  ;; Metamorphic -- How does changing the input, change its result?

  ;; Inductive

  ;; Model-based -- Abstract implementations to test the real
  (with-tests (:name "GENERATE-ENGLISH Oracle")
    (check-it (generator (string :min-length 2
				 :max-length *list-size*))
	      (lambda (s)
		(test (generate-phrase s)
		      (generate-english s)
		      :test #'equal))))
  (terpri)

  (with-tests (:name "GENERATE-GRAMMAR Oracle")
    (check-it (generator (string :min-length 2
				 :max-length *list-size*))
	      (lambda (s)
		(test (generate-phrase s)
		      (generate-grammar s)
		      :test #'equal))))
  (terpri)
  )

;; GENERATORS

;; HELPERS

;; (defun grammar-p (sentence)
;;   "Grammar is valid when either a noun-phrase is appended to a verb-phrase
;; or when just a phrase is returned.
;; Returns T if valid; Otherwise NIL
;; Sentence => Noun-Phrase + Verb-Phrase"
;;   (and (noun-phrase-p sentence)
;;        (verb-phrase-p sentence)))

;; (defun noun-phrase-p (noun-phrase)
;;   "A noun phrase is valid when it is made up of an Article prepended to a Noun.
;; Returns T; Otherwise NIL"
;;   (let ((construct (rewrites noun-phrase)))
;;     (dolist )))

;; (defun verb-phrase-p (verb-phrase)
;;   nil)

;; -------------------------------------------

(format t "~&Tests be run anytime with~%
(c2-examples) ; All Unit Tests~%
(c2-props)    ; Test all properties~%
or test everything~%
(progn (c2-examples) (c2-props))")
