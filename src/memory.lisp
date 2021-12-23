(uiop:define-package #:papi/memory
    (:use :cl #:40ants-doc)
  (:import-from #:papi/common :package+= :deft)
  (:use-reexport
   #+unix :papi/memory/unix
   #+windows :papi/memory/windows)
  (:documentation "Memory APIs (describe papi/memory::@doc)"))

(in-package #:papi/memory)

(defsection @doc (:title (package+= " documentation")
		  :export nil)
  ((translate-protection-flags) function)
#+c  (@examples section)
  (@os-specific-doc section))

