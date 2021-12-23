(uiop:define-package #:papi/sysinfo/windows
      (:use :cl #:40ants-doc)
  (:import-from #:cffi)
  (:import-from #:papi/common :package+=)
  (:import-from #:papi/windows :word :dword)
  (:export 
   #:get-system-info
   #:get-number-of-processors
   #:get-page-size
   
   #:@os-specific-doc))

(in-package #:papi/sysinfo/windows)

(defsection @os-specific-doc (:title (package+= " documentation"))
  ((get-system-info) function))

(cffi:defcstruct processor-struct
   (processor-architecture word)
   (reserved word))

(cffi:defcunion oem-union
   (oem-ide dword)
   (processor-struct (:struct processor-struct)))

(cffi:defcstruct system-info
   (oem-info (:union oem-union))
   (page-size dword)
   (minimum-application-address :pointer)
   (maximum-application-address :pointer)
   (active-processor-mask (:pointer dword))
   (number-of-processors dword)
   (processor-type dword)
   (allocation-granularity dword)
   (processor-level word)
   (processor-revision word))

(cffi:defcfun ("GetSystemInfo" get-system-info) :void
  (data (:pointer (:struct system-info))))

(defun get-number-of-processors ()
  (cffi:with-foreign-object (info '(:struct system-info))
    (get-system-info info)
    (cffi:foreign-slot-value info '(:struct system-info)
			     'number-of-processors)))

(defun get-page-size ()
  (cffi:with-foreign-object (info '(:struct system-info))
    (get-system-info info)
    (cffi:foreign-slot-value info '(:struct system-info)
			     'page-size)))
