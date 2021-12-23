(uiop:define-package #:papi/sysinfo
    (:use :cl #:40ants-doc)
  (:import-from #:papi/common :package+= :deft)
  (:use-reexport
   #+unix :papi/sysinfo/unix
   #+windows :papi/sysinfo/windows)
  (:documentation "System Information APIs (describe papi/sysinfo::@doc)"))

(in-package #:papi/sysinfo)

(defsection @doc (:title (package+= " documentation")
		  :export nil)
  ((get-number-of-processors get-page-size) function)
  (@examples section)
  (@os-specific-doc section))

(defsection @examples (:title (package+= " examples")
		       :export nil)
  ((:sysinfo) function))

(deft :sysinfo (:doc "Shows how to use the apis")
  (values
   (= 1 (signum (get-number-of-processors)))
   (= 1 (signum (get-page-size))))
  t t)
