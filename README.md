<a id='x-28GLITFENESTRO-2EDOCS-3A-40INDEX-20MGL-PAX-3ASECTION-29'></a>

# GLITFENESTRO: Sliding Window

## Table of Contents

- [1 Iterate Drivers][5a66]
    - [1.1 WINDOWING-BY][2efc]
    - [1.2 WINDOWING-IF][97c5]

###### \[in package GLITFENESTRO.DOCS with nicknames GFRO-DOCS\]
`GLITFENESTRO` is a package for analysis of sequences using sliding windows. A *window* or *frame* is a subsequence of consecutive elements sampled from the main sequence. Typical examples of applications of window functions include rolling averages, cumulative sums, and more complex things such as rolling regressions<sup>[1](#r-slider)</sup>.
<blockquote class="note">
GLITFENESTRO (*Esperanto*) <==> Sliding Window (*English*)
</blockquote>
This package contains,

1. drivers for the `iterate` package and

2. `map` functions

that provide flexible sliding window functionality for analyzing sequences. See GLITFENESTRO.DOCS:@API-REFERENCE for details.
  

<a id='x-28GLITFENESTRO-3A-40ITERATE-20MGL-PAX-3ASECTION-29'></a>

## 1 Iterate Drivers

###### \[in package GLITFENESTRO with nicknames GFRO\]
Two drivers are defined for the [iterate](https://common-lisp.net/project/iterate/doc/index.html) package,
**`WINDOWING-BY`** and **`WINDOWING-IF`**. **`WINDOWING-BY`** is for fixed or variable window size. **`WINDOWING-IF`** is for window sizes that satisfy a predicate.

<a id='x-28GLITFENESTRO-3A-40WINDOWING-BY-20MGL-PAX-3ASECTION-29'></a>

### 1.1 WINDOWING-BY

**DEFINITION**

```
FOR binding-form SLIDING-ACROSS seq WINDOWING-BY size SKIPPING-BY skip
```

**SYNOPSIS**

Iterate by fixed or variable window size sliding across a sequence

**`KEYWORD` ARGUMENTS**

`BINDING-FORM` -- a variable or two-variable proper list

`SEQ` -- a proper sequence

`SIZE` -- an integer or proper sequence of integers

`SKIP` -- an integer or proper sequence of integers

**`RETURN` `VALUES`**

For each iteration, a sequence of length `SIZE`.

**DESCRIPTION**

A single variable `BINDING-FORM` contains the window sub-sequence for each iteration. For a list pair `BINDING-FORM`, the first variable contains the window sub-sequence and the second is a list pair containing the window start and end indices.

This driver is useful when iterating a fixed or variable sliding window across a sequence. Each iteration returns a sub-sequence of size `SIZE` after skipping `SKIP` elements in the sequence (see examples for further details).

**NOTES**

The following is assumed in all examples that follow,

```
CL-USER> (ql:quickload :glitfenestro)
CL-USER> (ql:quickload :iterate)
CL-USER> (use-package :iterate)
```

<details open>
<summary> ***Basic Use*** </summary>

The most basic use is with fixed size and skip,

```cl-transcript
(iter
  (with seq = (alexandria:iota 20))
  (for w :sliding-across seq :windowing-by 2 :skipping-by 1)
  (collecting w))
=> ((0 1) (3 4) (6 7) (9 10) (12 13) (15 16) (18 19))

```

Functionality of many of `Serapeum`'s sequence functions can be obtained using this driver.

A `SKIP` of 0 yields output similar to the `serapeum:batches`,

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

The iterate driver is more flexible of course! For example, overlap can be obtained using a  *negative* `SKIP`,

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
<summary> ***Window Indices and Overhang*** </summary>

Both the `SIZE` and `SKIP` can be numeric sequences. In such cases windowing only occurs until the shorter of `SIZE` or `SKIP`.

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

The "full form" of the driver returns two values: (1) the window contents (2) the window extent - a list with the start and end indices of the window. Thus one could obtain the indices in the above example by using the regular destructuring pattern,

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

Of course the `SIZE` and `SKIP` lists can be heterogenous,

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
<summary> ***Aggregation*** </summary>

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
<summary> ***Recursion/Feedback*** </summary>

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

where the shorter of the `WINDOWING-BY` or `SKIPPING-BY` series determines the length of the generated sequence.
</details>

<details>
<summary> ***Scanning, Repeating, Cycling and Filtering*** </summary>

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
<summary> ***Sequence Types*** </summary>

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
  (with str = "The quick brown fox jumped over the lazy dogs.")
  (for w :sliding-across str
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> ("Th" "h" "" "e " "q" "ck ")

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
<summary> ***Nested Sequences*** </summary>

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
<summary> ***Edge Cases*** </summary>

An empty sequence returns `NIL`,
`cl-transcript
(iter
  (with seq = '())
  (for w :sliding-across seq
         :windowing-by '(2 1 0 2 1 3 2 2)
         :skipping-by '(-1 2 -2 0 2 -1 1))
  (collecting w))
=> NIL
`

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
(flet ((random-range (n) (- (random (1+ (* 2 n))) n))
       (loop-random (f n) (loop for i from 0 below 1000000 collect (funcall f n))))
  (time
    (iter
      (with seq = (make-array 1000000 :initial-contents (loop-random #'random 101)))
      (with win = (loop-random #'random 5))
      (with skip = (loop-random #'random-range 5))
      (for w :sliding-across seq :windowing-by win :skipping-by skip))))

.. Evaluation took:
..   0.145 seconds of real time
..   0.146188 seconds of total run time (0.137569 user, 0.008619 system)
..   [ Run times consist of 0.053 seconds GC time, and 0.094 seconds non-GC time. ]
..   100.69% CPU
..   336,167,382 processor cycles
..   73,595,808 bytes consed
=> NIL
```

```
(flet ((loop-random (f n) (loop for i from 0 below 1000000 collect (funcall f n))))
  (time
    (iter
      (with seq = (make-array 1000000 :initial-contents (loop-random #'random 101)))
      (for w :sliding-across seq :windowing-by 2 :skipping-by -1))))

.. Evaluation took:
..   0.224 seconds of real time
..   0.225691 seconds of total run time (0.176494 user, 0.049197 system)
..   [ Run times consist of 0.135 seconds GC time, and 0.091 seconds non-GC time. ]
..   100.89% CPU
..   518,013,836 processor cycles
..   88,001,104 bytes consed
=> NIL
```


<a id='x-28GLITFENESTRO-3A-40WINDOWING-IF-20MGL-PAX-3ASECTION-29'></a>

### 1.2 WINDOWING-IF

**DEFINITION**

```
FOR binding-form SLIDING-ACROSS seq WINDOWING-IF window-predicate
```

**SYNOPSIS**

Iterate by window consisting of consecutive elements satisifying `WINDOW-PREDICATE` sliding across a sequence

**`KEYWORD` ARGUMENTS**

`BINDING-FORM` -- a variable or two-variable proper list

`SEQ` -- a proper sequence

`WINDOW-PREDICATE` -- a designator for a function of one argument that returns a generalized boolean

**`RETURN` `VALUES`**

For each iteration, a sub-sequence of consecutive elements satisfying `WINDOW-PREDICATE`.

**DESCRIPTION**

A single variable `BINDING-FORM` contains the window sub-sequence for each iteration. For a list pair `BINDING-FORM`, the first variable contains the window sub-sequence and the second is a list pair containing the window start and end indices.

This driver is useful when iterating a sliding window across a sequence. Each iteration returns a sub-sequence consisting of consecutive elements that satisfy a given predicate.

**NOTES**

The following is assumed in all examples that follow,

```
CL-USER> (ql:quickload :glitfenestro)
CL-USER> (ql:quickload :iterate)
CL-USER> (use-package :iterate)
```

<details open>
<summary> ***Basic Use*** </summary>

The most basic use is with a sequence and a `WINDOWING-IF` predicate function,

```cl-transcript
(iter
  (with seq = '(1 3 5 2 6 7 9 4 8 4 3 2 8 8 8 9 0 0 2 4 5 3))
  (for w :sliding-across seq :windowing-if #'oddp)
  (collect w))
=> ((1 3 5) (7 9) (3) (9) (5 3))

```

The "full form" of the driver returns two values: (1) the window and (2) a 2 element list with the start and end indices of each window group,

```cl-transcript
(iter
  (with seq = '(1 1 1 1 2 2 2 3 3 3 2 2 4 4 4 1 1 5 5 5))
  (for (w i) :sliding-across seq :windowing-if #'evenp)
  (collect (list w i)))
=> (((2 2 2) (4 6)) ((2 2 4 4 4) (10 14)))

```

<details>
<summary> ***Run Length Encoding*** </summary>

The driver is very useful for sequences where repeated elements have to be categorized in some way. Consider the run length encoding (RLE) example below,

```cl-transcript
(iter
  (with seq = '(1 1 1 2 2 2 2 3 3 4 4 4 4 5 5 5 5 5 5 5 6 7 7))
  (with group-elt = (elt seq 0))
  (for (w i) :sliding-across seq
             :windowing-if (lambda(x) (= group-elt x)))
  (collect (list (car w) (length w)) into result)
  (for second-index = (cadr i))
  (unless (> (incf second-index) (length seq))
    (setf group-elt (elt seq second-index)))
  (finally (return result)))
=> ((1 3) (2 4) (3 2) (4 4) (5 7) (6 1) (7 2))

```

Another example from [Rosetta Code](https://www.rosettacode.org/wiki/Run-length_encoding) illustrates RLE on strings,

```
(iter
  (with str = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW")
  (with group-elt = (elt str 0))
  (for (w i) :sliding-across str
             :windowing-if (lambda(x) (equalp group-elt x)))
  (collect (list (elt w 0) (length w)) into result)
  (for second-index = (cadr i))
  (unless (>= (incf second-index) (length str))
    (setf group-elt (elt str second-index)))
  (finally (return result)))
=> ((#W 12) (#B 1) (#W 12) (#B 3) (#W 24) (#B 1) (#W 14))
```

</details>

<details>
<summary> ***Split Strings by Delimiter*** </summary>

A natural use case for the driver is to split strings by a delimiter,

```
(iter
  (with str = "The quick brown fox jumps over the lazy dog")
  (for w :sliding-across str
         :windowing-if (lambda (x) (not (equalp x #Space))))
  (collect w))
=> ("The" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog")
```

The `cl-ppcre` package is better suited for windowing by regex.

</details>

<details>
<summary> ***Filtering*** </summary>

The `WINDOWING-IF` predicate obviously lends itself to filtering functionality,

```cl-transcript
(iter
  (with mat = #(#(1 3 2) #(-1 0 -3) #(9 6 8)))
  (for w :sliding-across mat
         :windowing-if (lambda (x) (every #'plusp x)))
  (collect w))
=> (#(#(1 3 2)) #(#(9 6 8)))

```

</details>

<details>
<summary> ***Edge Cases*** </summary>

An empty sequence returns `NIL`,

```cl-transcript
(iter
  (with seq = '())
  (for w :sliding-across seq :windowing-if #'zerop)
  (collect w))
=> NIL

```

`NIL` is also returned if no contigous elements satisfy the predicate,

```cl-transcript
(iter
  (with seq = '(1 5 2 3 4))
  (for w :sliding-across seq :windowing-if #'zerop)
  (collect w))
=> NIL

```

</details>

**PERFORMANCE**

The driver is reasonably performant. An array sequence of 1 million elements takes less than 0.3s to process.

```
(flet ((random-range (n) (- (random (1+ (* 2 n))) n))
       (loop-random (f n) (loop for i from 0 below 1000000 collect (funcall f n))))
  (time
    (iter
      (with seq = (make-array 1000000 :initial-contents (loop-random #'random 101)))
       (for w :sliding-across seq :windowing-if #'plusp))))

.. Evaluation took:
..   0.125 seconds of real time
..   0.126364 seconds of total run time (0.074001 user, 0.052363 system)
..   [ Run times consist of 0.067 seconds GC time, and 0.060 seconds non-GC time. ]
..   100.80% CPU
..   289,933,222 processor cycles
..   32,118,304 bytes consed
=> NIL
```


<br> <br>

---

<a name="r-slider">1</a>: [Slider R Package](https://cran.r-project.org/web/packages/slider/vignettes/slider.html)
  

  [2efc]: #x-28GLITFENESTRO-3A-40WINDOWING-BY-20MGL-PAX-3ASECTION-29 "WINDOWING-BY"
  [5a66]: #x-28GLITFENESTRO-3A-40ITERATE-20MGL-PAX-3ASECTION-29 "Iterate Drivers"
  [97c5]: #x-28GLITFENESTRO-3A-40WINDOWING-IF-20MGL-PAX-3ASECTION-29 "WINDOWING-IF"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
