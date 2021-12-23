(uiop:define-package #:papi/memory/windows
    (:use #:cl #:40ants-doc)
  (:import-from #:papi/windows :dword :size_t)
  (:import-from #:papi/common :package+=)
  (:export 
   #:+PAGE-READWRITE+
   #:+PAGE-READONLY+
   #:+PAGE-EXECUTE-READWRITE+
   #:+PAGE-EXECUTE-READ+
   
   #:virtual-protect
   #:translate-protection-flags

   #:@os-specific-doc))

(in-package #:papi/memory/windows)

(defsection @os-specific-doc (:title (package+= " documentation"))
  ((+PAGE-READWRITE+
    +PAGE-READONLY+
    +PAGE-EXECUTE-READWRITE+
    +PAGE-EXECUTE-READ+) constant)
  ((virtual-protect) function))

(defconstant +page-execute-read+ 32)
(defconstant +page-execute-readwrite+ 64)
(defconstant +page-readonly+ 2)
(defconstant +page-readwrite+ 4)

(cffi:defcfun (virtual-protect "VirtualProtect") :boolean
  (address :pointer)
  (size size_t)
  (new-protect dword)
  (old-protect :pointer))

(defun translate-protection-flags (flags)
  "Flags can contain :read :write and/or :exec"
  (flet ((foundp (flag)
	   (find flag flags)))
    (cond ((every #'foundp (list :read :write :exec))
           +page-execute-readwrite+)
          ((every #'foundp (list :read :write))
           +page-readwrite+)
	  ((every #'foundp (list :read :exec)) 
	   +page-execute-read+)
	  ((every #'foundp (list :read))
	   +page-readonly+)
	  (T
	   (error "PROTECTION flags must contain :READ.")))))
