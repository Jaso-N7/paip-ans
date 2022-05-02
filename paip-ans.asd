(defsystem "paip-ans"
  :version "1.5.1"
  :author "Jason S. Robinson"
  :depends-on ()
  :components ((:file "models")
	       (:file "chap-1"
		:depends-on ("models")))
  :description "My personal answers to the PAIP Exercises"
  :in-order-to ((test-op (test-op "paip-ans/tests"))))

(defsystem "paip-ans/tests"
  :author "Jason S. Robinson"
  :depends-on ("paip-ans"
	       "cl-quickcheck")
 :components ((:module "tests"
		:components
		((:file "generators")
		 (:file "c1-unit")
		 (:file "c1-prop")
		 (:file "suite"))))
  :description "Test system for PAIP-ANS"
  :perform (test-op (op c)
		    (cl-quickcheck:quickcheck
		     (symbol-call :paip-ans/tests.suite
				  :run))))
