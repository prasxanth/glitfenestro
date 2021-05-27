(defpackage #:glitfenestro.tests
  (:nicknames #:gfro-tests)
  (:documentation "Unit tests for GLITFENESTRO")
  (:use #:cl #:fiveam #:glitfenestro #:iterate)
  (:import-from #:alexandria
		#:iota)
  (:shadowing-import-from #:generic-cl
			  #:equalp)
  (:export #:principal-suite
	   #:run-principal-suite))
