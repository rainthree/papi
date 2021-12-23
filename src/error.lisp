(uiop:define-package #:papi/error 
    (:use #:cl)
  (:use :cl #:40ants-doc)
  (:import-from #:papi/common :package+=)
  (:use-reexport #:papi/error/common
		 #+unix #:papi/error/unix
		 #+windows #:papi/error/windows
		 )
  (:export)
  (:documentation "Error handing APIs (describe papi/error::@doc)"))

(in-package #:papi/error)

(defsection @doc (:title (package+= " documentation"))  
  (papi/error/common::@doc section)
  (@os-specific-doc section)
  #+c(@examples section))

