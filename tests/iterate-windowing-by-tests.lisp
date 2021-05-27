(in-package :glitfenestro.tests)

(def-suite iterate-suite :in principal-suite)

(def-suite windowing-by-suite :in iterate-suite)

(in-suite windowing-by-suite)

(test iterate-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))

(defmacro build-test-expr (&key seq window skip)
  `(iter
     ;; ensure that all variables are evaluated only once
     (with seq% = ,seq)
     (with window% = ,window)
     (with skip% = ,skip)
     (for w :sliding-across seq%
	    :windowing-by window%
	    :skipping-by skip%)
     (collect w)))

(test basic-use
  "Basic use cases for the ITERATE WINDOWING-BY driver"

  (build-test-case
   :expr (build-test-expr :seq (iota 20) :window 2 :skip 1)
   :expected '((0 1) (3 4) (6 7) (9 10) (12 13) (15 16) (18 19))
   :description "WINDOWING-BY for (iota 20) list with window 2 and skip 1 should give contiguous subsequences of two elements skipping every (3*n - 1)th element")

  (build-test-case
   :expr (build-test-expr :seq (iota 20) :window 5 :skip 0)
   :expected '((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19))
   :description "WINDOWING-BY for (iota 20) list with skip 0 should replicate serapeum:batches"))
