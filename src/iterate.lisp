(in-package #:glitfenestro)

(defsection @iterate (:title "Iterate Drivers")
  "Two drivers are defined for the [iterate](https://common-lisp.net/project/iterate/doc/index.html) package,
   **WINDOWING-BY** and **WINDOWING-IF**. **WINDOWING-BY** is for fixed or variable window size. **WINDOWING-IF** is for window sizes that satisfy a predicate."

  (@windowing-by section)
  (@windowing-if section))

(defsection @windowing-by (:title "WINDOWING-BY")
  "
**DEFINITION**

```
FOR binding-form SLIDING-ACROSS seq WINDOWING-BY size SKIPPING-BY skip
```

**SYNOPSIS**

Iterate by fixed or variable window size sliding across a sequence

**KEYWORD ARGUMENTS**

BINDING-FORM -- a variable or two-variable proper list

SEQ -- a proper sequence

SIZE -- an integer or proper sequence of integers

SKIP -- an integer or proper sequence of integers

**RETURN VALUES**

For each iteration, a sequence of length SIZE.

**DESCRIPTION**

A single variable BINDING-FORM contains the window sub-sequence for each iteration. For a list pair BINDING-FORM, the first variable contains the window sub-sequence and the second is a list pair containing the window start and end indices.

This driver is useful when iterating a fixed or variable sliding window across a sequence. Each iteration returns a sub-sequence of size SIZE after skipping SKIP elements in the sequence (see examples for further details).

**NOTES**

<details open>
<summary> __*Basic Use*__ </summary>

The most basic use is with fixed size and skip,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for w :sliding-across seq :windowing-by 2 :skipping-by 1)
  (collecting w))
=> ((0 1) (3 4) (6 7) (9 10) (12 13) (15 16) (18 19))
```

Functionality of many of `Serapeum`'s sequence functions can be obtained using this driver.

A SKIP of 0 yields output similar to the `serapeum:batches`,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for w :sliding-across seq :windowing-by 5 :skipping-by 0)
  (collecting w))
=> ((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19))
```

```cl-transcript
(serapeum:batches (alexandria:iota 20) 5)
=> ((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19))
```

The iterate driver is more flexible of course! For example, overlap can be obtained using a  *negative* SKIP,

```cl-transcript
(iter
   (with seq = (alexandria:iota 10))
   (for w :sliding-across seq :windowing-by 3 :skipping-by -1)
   (collecting w))
=> ((0 1 2) (2 3 4) (4 5 6) (6 7 8))
```

```cl-transcript
(iter
   (with seq = (alexandria:iota 10))
   (for w :sliding-across seq :windowing-by 3 :skipping-by -2)
   (collecting w))
=> ((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8) (7 8 9))
```
</details>


<details>
<summary> __*Window Indices and Overhang*__ </summary>

Both the SIZE and SKIP can be numeric seqeunces. In such cases windowing only occurs until the shorter of SIZE or SKIP.

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for w :sliding-across seq
         :windowing-by '(2 2 2 2 2)
         :skipping-by '(1 1 1 1 1 1 1 1 1))
   (collecting w))
=> ((0 1) (3 4) (6 7) (9 10))
```

Termination at a length shorter than the input sequence results in an *overhang*, i.e., the end sub-sequence that is not part of the iteration.


The \"full form\" of the driver returns two values: (1) the window contents (2) the window extent - a list with the start and end indices of the window. Thus one could obtain the indices in the above example by using the regular destructuring pattern,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for (w i) :sliding-across seq
             :windowing-by '(2 2 2 2 2)
             :skipping-by '(1 1 1 1 1 1 1 1 1))
   (collecting (list w i)))
=> (((0 1) (0 1)) ((3 4) (3 4)) ((6 7) (6 7)) ((9 10) (9 10)))
```

As an example of the usefulness of the indices, consider extrating the overhang in the previous example,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (with max-index = 0)
  (for (w i) :sliding-across seq
	     :windowing-by '(2 2 2 2 2)
	     :skipping-by '(1 1 1 1 1 1 1 1 1))
  (collecting w into result)
  (when (> (cadr i) max-index) (setf max-index (cadr i)))
  (finally (return
	     (nconc result
		    (list (serapeum:slice seq
					  (1+ max-index)
					  (length seq)))))))
=> ((0 1) (3 4) (6 7) (9 10) (11 12 13 14 15 16 17 18 19))
```

The overhang is provided by default in `serapeum:batches`, while it needs to be explicitly obtained when using the driver as shown above.

Of course the SIZE and SKIP lists can be heterogenous,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for w :sliding-across seq
         :windowing-by '(2 3 0 1 1 2 3)
         :skipping-by '(-1 0 2 1 -2 0 3))
   (collecting w))
=> ((0 1) (1 2 3) NIL (6) (8) (7 8))
```
</details>

<details>
<summary> __*Aggregation*__ </summary>

Moving window aggregation is a very common operation in statistics. For example, the moving average (rolling average or running average) is used to analyze data points by creating a series of averages of different subsets of the full data set (see [Wikipedia](https://en.wikipedia.org/wiki/Moving_average)). A moving average with a window size of two for example is calculated as,

```cl-transcript
(iter
  (with seq = (alexandria:iota 10))
  (for w :sliding-across seq
         :windowing-by 2
         :skipping-by -1)
  (collecting (funcall #'alexandria:mean w)))
=> (1/2 3/2 5/2 7/2 9/2 11/2 13/2 15/2 17/2)
```

Likewise the moving window for any statistic can be obtained.

Differences between adjacent elements, called the first difference or successive difference, is used in calculation of the derivative,

```cl-transcript
(iter
  (with seq = '(1 3 2 5 6 7 4 8 0))
  (for w :sliding-across seq
         :windowing-by 2
         :skipping-by -1)
  (collecting (reduce #'- w)))
=> (-2 1 -3 -1 -1 3 -4 8)
```

Higher order derivatives can be calculated by repeating the above process using the sequence from previous differences.

</details>

<details>
<summary> __*Recursion/Feedback*__ </summary>

The sequence itself may be modified with each iteration.

One useful instance is when generating sequences by recursion, starting from some seed value as demonstrated using the Fibonacci sequence below,

```cl-transcript
(iter
  (with seq = '(0 1))
  (for w :sliding-across seq
         :windowing-by '(2 2 2 2 2 2 2)
         :skipping-by '(-1 -1 -1 -1 -1 -1 -1 -1))
  (for term = (reduce #'+ w))
  (collecting term)
  (setf seq (nconc seq (list term))))
=> (1 2 3 5 8 13)
```
where the shorter of the WINDOWING-BY or SKIPPING-BY series determines the length of the generated sequence.
</details>

<details>
<summary> __*Scanning, Repeating, Cycling and Filtering*__ </summary>

Interesting functionality can be obtained by simply varying the `SKIPPING-BY` sequence.

For example, `serapeum:scan` can be reproduced as,

```cl-transcript
(iter
  (with seq = (alexandria:iota 6))
  (for w :sliding-across seq
         :windowing-by '(1 2 3 4 5 6 7)
         :skipping-by '(-1 -2 -3 -4 -5 -6 -7))
  (collecting (reduce #'+ w)))
=> (0 1 3 6 10 15)
```

```
(serapeum:scan #'+ (alexandria:iota 6))
=> (0 1 3 6 10 15)
```

The driver is more flexible as it does not expect a reducing function. The equivalent of scanning using the `#'identity` function is easily realized,

```cl-transcript
(iter
  (with seq = (alexandria:iota 6))
  (for w :sliding-across seq
         :windowing-by '(1 2 3 4 5 6 7)
         :skipping-by '(-1 -2 -3 -4 -5 -6 -7))
  (collecting w))
=> ((0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4) (0 1 2 3 4 5))
```

Elements of a sequence can be selectively repeated. Consider repeating `2` twice and `4` four times in the sequence `0 1 2 3 4 5)`,

```cl-transcript
(iter
  (with seq = (alexandria:iota 6))
  (for w :sliding-across seq
         :windowing-by '(1 1 1 1 1 1 1 1 1 1 1)
         :skipping-by '(0 0 -1 0 0 -1 -1 -1 0 0 0))
  (nconcing w))
=> (0 1 2 2 3 4 4 4 4 5)
```

`serapeum:repeat-sequence` replicates the entire sequence,

```cl-transcript
(serapeum:repeat-sequence '(0 1 2 3 4 5) 3)
=> (0 1 2 3 4 5 0 1 2 3 4 5 0 1 2 3 4 5)
```

This can be easily reproduced,

```cl-transcript
(iter
  (with seq = (list (alexandria:iota 6)))
  (for w :sliding-across seq
         :windowing-by '(1 1 1 1)
         :skipping-by '(-1 -1 -1 0))
  (nconcing w into result)
  (finally (return (alexandria:flatten result))))
=> (0 1 2 3 4 5 0 1 2 3 4 5 0 1 2 3 4 5)
```

Sequence elements can be repeated to produce a cycling effect,

```cl-transcript
(iter
  (with seq = '(1 2 3))
  (for w :sliding-across seq
         :windowing-by '(3 3 3 3 3 3 3 3 3)
         :skipping-by '(-3 -3 -3 -3 -3 -3 -3 -3 -3))
  (nconcing w)
  (setq seq (nconc seq (list w))))
=> (1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)
```

The `SKIPPING-BY` parameter naturally provides position based filtering. Consider retaining only the elements divisible by three in a sequence from 1 to 10,

```cl-transcript
(iter
  (with seq = (alexandria:iota 10))
  (for w :sliding-across seq
         :windowing-by '(1 1 1 1 1)
         :skipping-by '(2 2 2 2 2))
  (nconcing w))
=> (0 3 6 9)
```

</details>

<details>
<summary> __*Sequence Types*__ </summary>

The driver works on any proper sequence. Examples for vectors, strings and bit-vectors are provided below,

*Vector*

```cl-transcript
(iter
  (with vec = #(0 1 2 3 4 5 6 7 8 9))
  (for w :sliding-across vec
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> (#(0 1) #(1) #() #(2 3) #(4) #(7 8 9))
```

*String*

```cl-transcript
(iter
  (with str = \"The quick brown fox jumped over the lazy dogs.\")
  (for w :sliding-across str
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> (\"Th\" \"h\" \"\" \"e \" \"q\" \"ck \")
```

*Bit-vector*

```cl-transcript
(iter
  (with bit-vec = #*11001010001)
  (for w :sliding-across bit-vec
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> (#*11 #*1 #* #*00 #*1 #*000)
```

Note that the return type is a list with elements of `(type input-sequence)`.

</details>


<details>
<summary> __*Nested Sequences*__ </summary>

Nested sequences are just another type of sequence so windowing will produce the expected results,

```cl-transcript
(iter
  (with seq = '((1 2) (3 4 5) (6 7 8 9) 0))
  (for w :sliding-across seq
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> (((1 2) (3 4 5)) ((3 4 5)) NIL ((6 7 8 9) 0))
```

</details>


<details>
<summary> __*Edge Cases*__ </summary>

An empty sequence returns `NIL`,
```cl-transcript
(iter
  (with seq = '())
  (for w :sliding-across seq
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> NIL
```

`WINDOWING-BY` must be an integer or a list of integers. The behavior for a `NIL` value is undefined.

```
(iter
  (with seq = '(1 2 3 4 5 6 7 8 9))
  (for w :sliding-across seq
         :windowing-by NIL
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> Undefined
```

An empty `SKIPPING-BY` value returns `NIL`.

```cl-transcript
(iter
  (with seq = '(1 2 3 4 5 6 7 8 9))
  (for w :sliding-across seq
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '())
  (collecting w))
=> NIL
```

A `NIL` value is returned when the starting window size is greater than the length of the sequence.

```cl-transcript
(iter
  (with seq = '(1 2 3 4 5 6 7 8 9))
  (for w :sliding-across seq
         :windowing-by 10
         :skipping-by 1)
  (collecting w))
=> NIL
```

A `NIL` value is also returned when the starting skip size is greater than the list.

```cl-transcript
(iter
  (with seq = '(1 2 3 4 5 6 7 8 9))
  (for w :sliding-across seq
         :windowing-by 0
         :skipping-by 10)
  (collecting w))
=> (NIL)
```

When the window size is the identical to the length of the sequence, the sequence (nested in a list) is returned.
Following are the results on a 64bit Mac with a 2.3 GHz 8-Core Intel Core i9 processor and 16 GB 2667 MHz DDR4 of RAM.

```cl-transcript
(iter
  (with seq = '(1 2 3 4 5 6 7 8 9))
  (for w :sliding-across seq
         :windowing-by '(9 2)
         :skipping-by 10)
  (collecting w))
=> ((1 2 3 4 5 6 7 8 9))
```

</details>

**PERFORMANCE**


The driver is reasonably performant. An array sequence of 1 million elements takes less than 0.3s to process.


```
(flet ((random-range (n) (- (random (1+ (* 2 n))) n)))
  (time
    (iter
      (with seq = (make-array 1000000 :initial-contents (loop for i from 0 below 1000000 collect (random 101))))
      (with win = (loop for i from 0 below 1000000 collect (random 5)))
      (with skip = (loop for i from 0 below 1000000 collect (random-range 5)))
      (for w :sliding-across seq :windowing-by win :skipping-by skip))))

.. Evaluation took:
..   0.142 seconds of real time
..   0.143495 seconds of total run time (0.136580 user, 0.006915 system)
..   [ Run times consist of 0.055 seconds GC time, and 0.089 seconds non-GC time. ]
..   100.70% CPU
..   328,242,728 processor cycles
..   73,693,568 bytes consed
=> NIL
```

```
(time
  (iter
    (with seq = (make-array 1000000 :initial-contents (loop for i from 0 below 1000000 collect (random 101))))
    (for w :sliding-across seq :windowing-by 2 :skipping-by -1)))

.. Evaluation took:
..   0.114 seconds of real time
..   0.114921 seconds of total run time (0.097959 user, 0.016962 system)
..   [ Run times consist of 0.023 seconds GC time, and 0.092 seconds non-GC time. ]
..   100.88% CPU
..   264,150,066 processor cycles
..   71,985,760 bytes consed
=> NIL
```

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  WINDOWING-BY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro-driver (FOR binding-form
		      SLIDING-ACROSS seq
		      WINDOWING-BY size
		      SKIPPING-BY skip)
  "See `@windowing-by` for documentation and examples."
  (destructuring-bind (var &rest indices
		       &aux (kwd (if generate 'generate 'for)))
      (ensure-list binding-form)
    (with-gensyms (size% length-size% skip% length-skip% x% z% k%)
      `(progn
	 (with ,size% = (etypecase ,size
			  (integer (make-list (length ,seq)
					      :initial-element ,size))
			  (sequence ,size)))
	 (with ,skip% = (etypecase ,skip
			  (integer (make-list (length ,seq)
					      :initial-element ,skip))
			  (sequence ,skip)))
	 (with ,x% = 0)  ; window start index
	 (with ,length-size% = (length ,size%))
	 (with ,length-skip% = (length ,skip%))
	 (with ,z% = (pop ,size%))
	 (with ,k% = (pop ,skip%))
	 (,kwd (,var ,@indices) next
	       (prog2
		   (when (or (> (+ ,x% ,z%) (length ,seq))
			     (>= 0 (decf ,length-size%))
			     (>= 0 (decf ,length-skip%))) (terminate))
		   (list (slice ,seq ,x% (+ ,x% ,z%))
			 (list ,x% (1- (+ ,x% ,z%))))
		 (setf ,x% (+ ,x% ,z% ,k%)
		       ,z% (pop ,size%)
		       ,k% (pop ,skip%))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  WINDOWING-IF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (function sequence integer) (integer)) position-while))
(defun position-while (stop-predicate seq start)
  "
**SYNOPSIS**

Return (1+ last-consecutive-index) from start for which stop-predicate is true

**KEYWORD ARGUMENTS**

STOP-PREDICATE -- a designator for a function of one argument that returns a generalized boolean

SEQ -- a proper sequence

START -- an integer

**RETURN VALUES**

INDEX -- an integer

**DESCRIPTION**

Consecutive elements of sequence SEQ starting at index START are tested against STOP-PREDICATE. The (1+ last-index) for which the predicate is true is returned. Hence, the consecutive elements between the two indices are those that satisfy the predicate.

The return value of (1+ last-index) is convenient to use in functions such as `serapeum:slice` which is *exclusive* as the element corresponding to the last-index is not included.

```
(serapeum:slice (alexandria:iota 10) 5 8)
=> (5 6 7)
```

**NOTES**

A basic use case demonstrated below returns the last index of the consecutive even number starting at the first `4` in the sequence,
```cl-transcript
(let ((seq '(1 1 1 2 2 0 3 4 4 4 4 5 6 6 6 6 6)))
  (position-while #'evenp seq 7))
=> 11
```

The starting index is returned if,

* The predicate is not satisfied for the very next element,

```cl-transcript
(let ((seq '(1 1 1 2 2 0 3 4 4 4 4 5 6 6 6 6 6)))
  (position-while #'zerop seq 1))
=> 1
```

* Or the input sequence is empty,

```cl-transcript
(let ((seq '()))
  (position-while #'zerop seq 4))
=> 4
```
  "
  (iter
    (with len-seq% = (length seq))
    (for index% :from start :to (1- len-seq%))
    (for element = (elt seq index%))
    (while (funcall stop-predicate element))
    (finally (return index%))))

(defsection @windowing-if (:title "WINDOWING-IF")
  "
**DEFINITION**

```
FOR binding-form SLIDING-ACROSS seq WINDOWING-IF window-predicate
```

**SYNOPSIS**

Iterate by window consisting of consecutive elements satisifying `WINDOW-PREDICATE` sliding across a sequence

**KEYWORD ARGUMENTS**

BINDING-FORM -- a variable or two-variable proper list

SEQ -- a proper sequence

WINDOW-PREDICATE -- a designator for a function of one argument that returns a generalized boolean

**RETURN VALUES**

For each iteration, a sub-sequence of consecutive elements satisfying `WINDOW-PREDICATE`.

**DESCRIPTION**

A single variable BINDING-FORM contains the window sub-sequence for each iteration. For a list pair BINDING-FORM, the first variable contains the window sub-sequence and the second is a list pair containing the window start and end indices.

This driver is useful when iterating a sliding window across a sequence. Each iteration returns a sub-sequence consisting of consecutive elements that satisfy a given predicate.

**NOTES**

<details open>
<summary> __*Basic Use*__ </summary>

```cl-transcript
(iter
  (with seq = '(1 1 1 1 2 2 2 3 3 3 2 2 4 4 4 1 1 5 5 5))
  (for (w i) :sliding-across seq :windowing-if #'evenp)
  (collect (list w i)))
=> (((2 2 2) (4 6)) ((2 2 4 4 4) (10 14)))
```

```cl-transcript
(iter
  (with seq = '(1 3 5 2 6 7 9 4 8 4 3 2 8 8 8 9 0 0 2 4 5 3))
  (for w :sliding-across seq :windowing-if #'oddp)
  (collect w))
=> ((1 3 5) (7 9) (3) (9) (5 3))
```
Run length encoding

```cl-transcript
(iter
  (with seq = '(1 1 1 2 2 2 2 3 3 4 4 4 4 5 5 5 5 5 5 5 6 7 7))
  (with group-elt = (elt seq 0))
  (for (w i) :sliding-across seq :windowing-if (lambda(x) (= group-elt x)))
  (collect (list (car w) (length w)) into result)
  (for second-index = (cadr i))
  (unless (> (incf second-index) (length seq))
    (setf group-elt (elt seq second-index)))
  (finally (return result)))
=> ((1 3) (2 4) (3 2) (4 4) (5 7) (6 1) (7 2))
```

RLE Rosetta Code problem

```
(iter
  (with str = \"WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW\")
  (with group-elt = (elt str 0))
  (for (w i) :sliding-across str
             :windowing-if (lambda(x) (equalp group-elt x)))
  (collect (list (elt w 0) (length w)) into result)
  (for second-index = (cadr i))
  (unless (>= (incf second-index) (length str))
    (setf group-elt (elt str second-index)))
  (finally (return result)))
=> ((#\W 12) (#\B 1) (#\W 12) (#\B 3) (#\W 24) (#\B 1) (#\W 14))
```


Split string by delimiter or regex,

```
(iter
  (with str = \"The quick brown fox jumps over the lazy dog\")
  (for w :sliding-across str
         :windowing-if (lambda (x) (not (equalp x #\Space))))
  (collect w))
=> (\"The\" \"quick\" \"brown\" \"fox\" \"jumps\" \"over\" \"the\" \"lazy\" \"dog\")
```

Binary to decimal

```
(iter
  (with bit-vector = #*110110011)
  (with bit-len = (1- (length bit-vector)))
  (for (w i) :sliding-across bit-vector
             :windowing-if (lambda (x) (= x 1)))
  (for start = (car i))
  (for stop = (cadr i))
  (for n = (1+ (- stop start)))
  (nconcing (mapcar (lambda (x) (expt 2 (- bit-len x)))
     (alexandria:iota n :start start :step 1)) into result)
  (finally (return (reduce #'+ result))))
=> 435
```

Filter rows or columns a matrix,

```cl-transcript
(iter
  (with mat = #(#(1 3 2) #(-1 0 -3) #(9 6 8)))
  (for w :sliding-across mat :windowing-if (lambda (x) (every #'plusp x)))
  (collect w))
=> (#(#(1 3 2)) #(#(9 6 8)))
```

Edge cases, nil sequence returns nil


```cl-transcript
(iter
  (with seq = '())
  (for w :sliding-across seq :windowing-if #'zerop)
  (collect w))
=> NIL
```

NIL returned if no contigous elements satisfies condition
```cl-transcript
(iter
  (with seq = '(1 5 2 3 4))
  (for w :sliding-across seq :windowing-if #'zerop)
  (collect w))
=> NIL
```

</details>

**PERFORMANCE**

```
(flet ((random-range (n) (- (random (1+ (* 2 n))) n)))
  (time
    (iter
      (with seq = (make-array 1000000 :initial-contents (loop for i from 0 below 1000000 collect (random 101))))
       (for w :sliding-across seq :windowing-if #'plusp))))

.. Evaluation took:
..   0.191 seconds of real time
..   0.164019 seconds of total run time (0.099364 user, 0.064655 system)
..   [ Run times consist of 0.113 seconds GC time, and 0.052 seconds non-GC time. ]
..   85.86% CPU
..   440,164,310 processor cycles
..   32,110,528 bytes consed
=> NIL
```
"
  )

(defmacro-driver (FOR binding-form
		      SLIDING-ACROSS seq
		      WINDOWING-IF window-predicate)
  "See `@windowing-if` for documentation and examples."
  (destructuring-bind (var &rest indices
		       &aux (kwd (if generate 'generate 'for)))
      (ensure-list binding-form)
    (with-gensyms (window-start-index% window-end-index%)
      `(progn
	 (with ,window-start-index% = 0)
	 (with ,window-end-index% = 0)
	 (,kwd (,var ,@indices) next
	       (progn
		 (setf ,window-start-index%
		       (position-if ,window-predicate
				    ,seq
				    :start ,window-end-index%))
		 (unless ,window-start-index% (terminate))
		 (setf ,window-end-index%
		       (position-while ,window-predicate
				       ,seq
				       ,window-start-index%))
		 (list (slice ,seq ,window-start-index% ,window-end-index%)
		       (list ,window-start-index% (1- ,window-end-index%)))))))))