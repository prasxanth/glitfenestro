(in-package #:glitfenestro.docs)

(defsection @index (:title "GLITFENESTRO: Sliding Window")
  "
GLITFENESTRO is a package for analysis of sequences using sliding windows. A *window* or *frame* is a subsequence of consecutive elements sampled from the main sequence. Typical examples of applications of window functions include rolling averages, cumulative sums, and more complex things such as rolling regressions<sup>[1](#r-slider)</sup>.

<blockquote class=\"note\">
\\GLITFENESTRO (*Esperanto*) <==> Sliding Window (*English*)
</blockquote>

This package contains,

1. drivers for the `iterate` package and
2. `map` functions

that provide flexible sliding window functionality for analyzing sequences. See GLITFENESTRO.DOCS:@API-REFERENCE for details.
  "
  (@iterate section)

  "
<br> <br>

--------

<a name=\"r-slider\">1</a>: [Slider R Package](https://cran.r-project.org/web/packages/slider/vignettes/slider.html)
  "
  )
