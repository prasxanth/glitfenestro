(in-package :glitfenestro.tests)

(def-suite iterate-suite :in principal-suite)

(in-suite iterate-suite)

(test iterate-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
