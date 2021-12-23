(uiop:define-package #:papi/file/windows
    (:use :cl #:40ants-doc)
  (:import-from #:cffi)
  (:import-from #:papi/common :translate-path :package+=)
  (:import-from #:papi/error :error-when :error-unless)
  (:import-from #:papi/windows :lpsecurity-attributes :wchar_t :handle :dword :size_t :large-integer)
  (:export #:+generic-read+
	   #:+generic-write+
	   #:+invalid-file-size+
	   
	   #:+INVALID-HANDLE-VALUE+
	   	   
	   #:+create-new+
	   #:+create-always+
	   #:+OPEN-EXISTING+
	   #:+OPEN-ALWAYS+
	   #:+TRUNCATE-EXISTING+	   

	   #:+file-attribute-normal+
	   #:+file-flag-no-buffering+
	   #:+file-flag-write-through+

	   #:+file-share-delete+
	   #:+file-share-read+
	   #:+file-share-write+

	   #:translate-open-access
	   #:translate-open-disposition
	   #:translate-open-flags
	   #:translate-open-share

	   #:create-file
	   #:close-handle
	   #:close-file
	   #:flush-file-buffers
           #:get-file-size-ex
	   #:open-file%
	   #:open-file
	   #:get-file-size

	   #:@os-specific-doc))

(in-package #:papi/file/windows)

(defsection @os-specific-doc (:title (package+= " documentation"))
  "Constants"
  (+generic-read+  constant)
  (+generic-write+  constant)
  (+invalid-file-size+ constant)
  (+INVALID-HANDLE-VALUE+ constant)
	   	   
  (+create-new+ constant)
  (+create-always+ constant)
  (+OPEN-EXISTING+ constant)
  (+OPEN-ALWAYS+ constant)
  (+TRUNCATE-EXISTING+	    constant)

  (+file-attribute-normal+ constant)
  (+file-flag-no-buffering+ constant)
  (+file-flag-write-through+ constant)

  (+file-share-delete+ constant)
  (+file-share-read+ constant)
  (+file-share-write+ constant)

  (translate-open-access function)
  (translate-open-disposition function)
  (translate-open-flags function)
  (translate-open-share function)

  (create-file function)
  (close-handle function)
  (flush-file-buffers function)
  (get-file-size-ex function)
  (open-file% function)
  ;; these are documented by the module (parent package)
  ;; close-file
  ;; open-file
  ;; get-file-size
  )

(defconstant +generic-read+  #x80000000)
(defconstant +generic-write+ #x40000000)
(defconstant +invalid-file-size+  #xFFFFFFFF)
(defconstant +invalid-handle-value+
  (or (and (boundp '+invalid-handle-value+)
	   (symbol-value '+invalid-handle-value+))
          #+x86-64 (cffi:make-pointer (ldb (byte 64 0) -1))
          #+x86 (cffi:make-pointer (ldb (byte 32 0) -1))))
(defconstant +create-new+ 1)
(defconstant +create-always+ 2)
(defconstant +open-existing+ 3)
(defconstant +open-always+ 4)
(defconstant +truncate-existing+ 5)

(defconstant +file-attribute-normal+  #x00000080)
(defconstant +file-flag-no-buffering+ #x20000000)
(defconstant +file-flag-write-through+ #x80000000)

(defconstant +file-share-delete+ 4)
(defconstant +file-share-read+ 1)
(defconstant +file-share-write+ 2)

(cffi:defcfun (create-file "CreateFileW") handle
  (path :pointer)
  (access dword)
  (share-mode dword)
  (attributes lpsecurity-attributes)
  (creation-disposition dword)
  (flags-and-attributes dword)
  (template-file handle))

(cffi:defcfun (get-file-size-ex "GetFileSizeEx") :boolean
  (file handle)
  (file-size :pointer))

(cffi:defcfun (close-handle "CloseHandle") :boolean
  (object handle))

(cffi:defcfun (flush-file-buffers "FlushFileBuffers") :boolean
  (file handle))

(defun translate-open-access (flags)
  (logior
   (or (and (find :read flags)
	    +generic-read+)
       (error "OPEN flags must contain :READ."))
   (or (and (find :write flags)
	    +generic-write+)
       0)))

(defun translate-open-share (flags)
  (logior
   (or (and (find :delete flags)
	    +file-share-delete+)
       0)
   (or (and (find :read flags)
	    +file-share-read+)
       0)
   (or (and (find :write flags)
	    +file-share-write+)
       0)))

(defun translate-open-disposition (flags)
  (or (and (find :create flags)
	   (or (and (find :ensure-create flags)
		    +create-new+)
	       +open-always+))
      (or (and (find :truncate flags)
	       +truncate-existing+)
	   +open-existing+)))

(defun translate-open-flags (flags)
  (logior +file-attribute-normal+
	  (or (and (find :direct flags)
		   +file-flag-no-buffering+)
	      0)
	  (or (and (find :file-sync flags)
		   +file-flag-write-through+)
	      0)))

(defun open-file% (path
		   open-access open-share open-security
		   open-disposition open-flags
		   template-file-fd)
  "Tiny wrapper over CreateFileW winapi."
  (let ((fd +invalid-handle-value+))
    (declare (type cffi:foreign-pointer fd))
    (declare (type string path))
    (cffi:with-foreign-string (string path :encoding :utf-16)
      (setf fd (create-file string
                            open-access
                            open-share
                            open-security
                            open-disposition
                            open-flags
                            template-file-fd)))
    (error-when (cffi:pointer-eq fd +invalid-handle-value+))
    fd))

(defun open-file (path &key (open '(:read))
			 (share '(:read))
			 (security (cffi:null-pointer))
			 (template-file (cffi:null-pointer)))
  "High-level wrapper over CreateFileW winapi."
  (open-file% (translate-path path)
              (translate-open-access open)
	      (translate-open-share share)
	      security
              (translate-open-disposition open)
              (translate-open-flags open)
	      template-file))

(defun get-file-size (fd)
  (let (size)
    (declare (type (or null (unsigned-byte 64)) size))
    (cffi:with-foreign-object (p-size 'large-integer)     
      (error-unless (get-file-size-ex fd p-size))
      (setf size (cffi:mem-ref p-size 'large-integer)))))

(defun close-file (fd)
  (close-handle fd))
