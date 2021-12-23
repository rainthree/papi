(uiop:define-package #:papi/all
    (:use :cl)
  (:import-from #:40ants-doc :defsection)
  #+asdf-load-this-manually(:import-from #:40ants-doc/builder)
  (:import-from #:papi/init)
  (:import-from #:papi/common :package+=)
  (:import-from #:papi/sysinfo)
  (:import-from #:papi/memory)
  (:use-reexport #:papi/file/all)
  (:export :gendoc :gendoc-base))

(in-package #:papi/all)

(defsection @doc-base (:title (package+= " documentation") :ignore-words ())
  "## Project Structure
    ### papi/common has utilities common to multiple modules of all different operating systems
    ### papi/windows has definitions and utilities common to different modules/apis for Windows
    ### papi/file file operations module: open-file close-file get-file-size etc
     #### papi/file/windows.lisp has the windows specific definitions for this module
     #### papi/file/unix.lisp has the unix specific definitions for this module
    ### papi/file/map file mapping module:
     #### papi/file/windows.lisp has the windows specific definitions for this module
     #### papi/file/unix.lisp has the unix specific definitions for this module
    ### papi/file/all uses and re-exports all file modules (papi/file, papi/file/map,etc) 
```lisp
(asdf:load-system :papi)
;;; if you want to view some module's documentation in the repl:
(describe papi/memory::@doc)
;;; or if you want to generate the documentation:
;;; first clone https://github.com/rainthree/doc or https://github.com/40ants/doc/ (see which one is the latest)
(ql:quickload '(:papi :40ants-doc-full))
(papi/all:gendoc)
;;; note that I had to comment download-highlight-js in doc/src/themes/api.lisp because I was getting a https error.
;;; to generate only this basic readme:
(papi/all:gendoc-base)
;;; Testing
(asdf:test-system :papi)
;;; Thanks to :package-inferred-system you can also test individual modules:
(asdf:load-system :papi/error/windows) ; The tests are defined in the sourcefiles so rt::*entries* will be populated when loading the system.
(rt:do-tests)
(rt:rem-all-tests) ; If afterwards you need to test other systems that use RT (such as ironclad), then first call (rt:rem-all-tests)
(asdf:load-system :papi/error/windows :force '(:papi/error/windows))
(rt:do-tests) ; test again
```
RT is used for testing, see 'Supporting the Regression Testing of Lisp Programs'  https://3e8.org/pub/scheme/doc/lisp-pointers/v4i2/p47-waters.pdf
'When switching from testing one system to testing another, it is wise to remove all the old tests before beginning to define new ones.'
")

(defsection #+unix @unix #+windows @windows
  (:title (package+= " documentation") :ignore-words ())
  (papi system)
  (@doc-base section)
  (papi/common::@doc section)
  (papi/error::@doc section)
  (papi/sysinfo::@doc section)
  (papi/memory::@doc section)
  (papi/file::@doc section)
  (papi/file/map::@doc section))

(defun gendoc ()
  (uiop:symbol-call "40ANTS-DOC/BUILDER" "UPDATE-ASDF-SYSTEM-DOCS"
   #+unix @unix #+windows @windows
   :papi))

(defun gendoc-base ()
  (uiop:symbol-call "40ANTS-DOC/BUILDER" "UPDATE-ASDF-SYSTEM-DOCS"
		    papi/all::@doc-base :papi))
