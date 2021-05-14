(defpackage #:glitfenestro.docs
  (:nicknames #:gfro-docs)
  (:use #:cl #:iterate)
  (:import-from #:mgl-pax
                #:section
                #:defsection)
  (:import-from #:glitfenestro
		#:@iterate
		#:@windowing-by)
  (:export #:build
	   #:@index))
