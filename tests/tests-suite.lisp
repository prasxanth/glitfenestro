(in-package #:glitfenestro.tests)

(def-suite principal-suite
  :description "The master suite of all tests. Precipa translates to principal in Esperanto.")

(in-suite principal-suite)

(defun run-principal-suite ()
  (run! 'principal-suite))

(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
