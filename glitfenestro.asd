;;;; glitfenestro.asd

;; (ql:quickload "glitfenestro")
(asdf:defsystem "glitfenestro"
  :class :package-inferred-system
  :depends-on ("iterate" "alexandria" "serapeum" "mgl-pax")
  :author "Prashanth Kumar <prasxanth.kumar@gmail.com>"
  :license  "Unlicense"
  :version "1.0"
  :description "Iterate and apply functions to sub-sequences obtained from sliding a window across an underlying sequence."
  :serial t
  :pathname "src/"
  :components ((:file "package")
	       (:file "iterate" :depends-on ("package")))
  :in-order-to ((test-op (test-op "glitfenestro/tests"))))


(asdf:defsystem "glitfenestro/tests"
  :depends-on ("glitfenestro" "fiveam" "iterate" "alexandria" "generic-cl")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
	       (:file "tests-suite" :depends-on ("package"))
	       (:file "iterate-windowing-by-tests" :depends-on ("package" "tests-suite")))
  :perform (test-op (o c) (symbol-call :glitfenestro.tests
				       :run-principal-suite)))

;; (ql:quickload "glitfenestro/docs")
(asdf:defsystem "glitfenestro/docs"
  :class :package-inferred-system
  :depends-on ("glitfenestro" "iterate" "alexandria" "serapeum")
  :pathname "docs/"
  :serial t
  :components ((:file "package")
	       (:file "build")
	       (:file "docs")
	       (:static-file "styles.css")
	       (:static-file "jquery.min.js")
	       (:static-file "toc.min.js")))
