(in-package #:glitfenestro.docs)

(defun build ()
  "
**Synopsis** -- Generate MGL-PAX documentation from docstrings and documentation files

**Input Arguments**

No input arguments.

**Return Values**

No return values.

**Description**

Uses `MGL-PAX:UPDATE-ASDF-SYSTEM*` to generate,

* `README` and `README.md` in the project root directory
* `index.html` in the `docs/` sub-directory.

**Examples**

```
(ql:quickload :glitfenestro/docs)
(gfro-docs:build)
```
  "

  ;; Update READMEs
  (mgl-pax:update-asdf-system-readmes @index :glitfenestro)

  ;; Update html file only
  (mgl-pax:update-asdf-system-html-docs
   @index :glitfenestro
   :target-dir "../docs/"
   :update-css-p nil
   :pages `((:objects (,glitfenestro.docs:@index)))))
