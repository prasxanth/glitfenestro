(defpackage #:glitfenestro
  (:nicknames #:gfro)
  (:use #:cl #:iterate)
  (:import-from #:alexandria
		#:with-gensyms
		#:ensure-list
		#:iota)
  (:import-from #:serapeum
		#:slice)
  (:import-from #:mgl-pax
		#:section
                #:defsection)
  (:export #:sliding-across
	   #:windowing-by
	   #:skipping-by
	   ;; documentation
	   #:@iterate
	   #:@windowing-by)
  (:documentation "Docstring for the package
                  "))
