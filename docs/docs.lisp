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
  (@asdf-details section)
  (@system-details section)
  (@installation section)
  (@iterate section)

  "
<br> <br>

--------

<a name=\"r-slider\">1</a>: [Slider R Package](https://cran.r-project.org/web/packages/slider/vignettes/slider.html)
  "
  )


(defsection @asdf-details (:title "ASDF Details")
  "
  - **Version**: 1.0.0
  - **Description**: Skeleton template for Lisp project with test and documentation generator.
  - **License**: Unlicense.
  - **Author**: Prashanth Kumar
  - **Mailto**: prasxanth.kumar@gmail.com
  - **Homepage**: http://prasxanth.github.io/glitfenestro/
  - **Bug tracker**: https://github.com/prasxanth/glitfenestro/issues
  - **Source control**: http://github.com/glitfenestro.git

  ")


(defsection @system-details (:title "System Details")
  "
This package was developed and tested on,

  - **OS**: macOS Big Sur
  - **Processor**: 2.3 GHz 8-Core Intel Core i9
  - **Memory**: 16 GB 2667 MHz DDR4
  - **Lisp**: SBCL 2.1.3
  - **IDE**: EMACS SLIME

  ")


(defsection @installation (:title "Installation")
  "

This package is not available through `QUICKLISP` (yet), so it must be manually loaded. To do so, create a softlink to `~/quicklisp/local-projects`,

```
ln -s ~/glitfenestro/source/location ~/quicklisp/local-projects/glitfenestro
```

Quickload the project in SLIME to make it available,

```
(ql:quickload :glitfenestro)
```

The `iterate` package is a dependency,

```
(ql:quickload :iterate)
(use-package :iterate)
```

To run the tests,

```
(asdf:test-system :glitfenestro)
```

To generate the documentation,

```
(ql:quickload :glitfenestro/docs)
(gfro-docs:build)
```
  ")
