(uiop:define-package #:papi/file/map/windows
    (:use #:cl)
  (:import-from #:40ants-doc :defsection)
  (:import-from #:cffi)
  (:import-from #:papi/windows #:lpsecurity-attributes #:wchar_t #:handle #:dword #:size_t #:large-integer)
  (:import-from #:papi/error #:api-error #:error-when #:error-unless)
  (:import-from #:papi/common :package+= :deft :translate-path)
  (:import-from #:papi/memory/windows #:virtual-protect #:translate-protection-flags)
  (:import-from #:papi/file #:close-handle #:open-file #:get-file-size #:+invalid-handle-value+)
  (:export 
   #:+file-map-copy+
   #:+file-map-execute+
   #:+file-map-read+
   #:+file-map-write+
        
   #:create-file-mapping
   #:map-view-of-file
   #:unmap-view-of-file
   #:flush-view-of-file
   
   #:translate-mmap-access-flags
   #:mprotect
   #:mmap
   #:munmap
   #:msync

   #:@os-specific-doc))

(in-package #:papi/file/map/windows)

(defsection @os-specific-doc (:title (package+= " documentation")
			      :export nil)
  ((+file-map-copy+
   +file-map-execute+
   +file-map-read+
   +file-map-write+) constant)
  ((create-file-mapping
   map-view-of-file
   unmap-view-of-file
   flush-view-of-file
   translate-mmap-access-flags
   mprotect
   mmap
   munmap
   msync) function)
  #+c(@examples section))

(defconstant +file-map-copy+ 1)
(defconstant +file-map-execute+ 32)
(defconstant +file-map-read+ 4)
(defconstant +file-map-write+ 2)

(cffi:defcfun (create-file-mapping "CreateFileMappingA") handle
  (file handle)
  (attributes lpsecurity-attributes)
  (protect dword)
  (maximum-size-high dword)
  (maximum-size-low dword)
  (name :pointer))

(cffi:defcfun (map-view-of-file "MapViewOfFile") :pointer
  (file-mapping-object handle)
  (desired-access dword)
  (file-offset-high dword)
  (file-offset-low dword)
  (number-of-bytes-to-map size_t))

(cffi:defcfun (unmap-view-of-file "UnmapViewOfFile") :boolean
  (base-address :pointer))

(cffi:defcfun (flush-view-of-file "FlushViewOfFile") :boolean
  (base-address :pointer)
  (number-of-bytes-to-flush size_t))

(cffi:defcfun (flush-file-buffers "FlushFileBuffers") :boolean
  (file handle))

(defun translate-mmap-access-flags (protection flags)
  (unless (or (find :private flags)
	      (find :shared flags))
    (error "MMAP flags must contain either :PRIVATE or :SHARED."))
  (logior (or (and (find :write protection)
		   +file-map-write+)  
	      +file-map-read+)
	  (or (and (find :exec protection)
		   +file-map-execute+)
	      0)
	  (or (and (find :private protection)
		   +file-map-copy+)
	      0)))

(defun mmap%% (fd size offset protection map-access)
  (declare (type fixnum offset protection map-access))
  (declare (type (or null (unsigned-byte 64)) size))
  #+(or)(declare (optimize speed))
  (when (null size)
    (setf size (get-file-size fd)))
  (let* ((end (+ size offset))
         (map-handle (create-file-mapping fd
                                      (cffi:null-pointer) ; LPSECURITY_ATTRIBUTES
                                      protection
                                      (ldb (byte 32 32) end)
                                      (ldb (byte 32 0) end)
                                      (cffi:null-pointer))) ; lpName
         (pointer (map-view-of-file map-handle
                                    map-access
                                    (ldb (byte 32 32) offset)
                                    (ldb (byte 32 0) offset)
                                    size)))
    (declare (type (unsigned-byte 64) end))
    (handler-bind ((api-error (lambda (e)
                                 (declare (ignore e))
                                 (close-handle map-handle)
                                 (unless (cffi:pointer-eq +invalid-handle-value+ fd)
                                   (close-handle fd)))))
      (error-when (cffi:null-pointer-p pointer))
      (values pointer (cons fd map-handle) size))))

(defun mmap% (fd &key (protection '(:read))
		   (mmap-access '(:private))
		   size
		   (offset 0))
  (mmap%% fd
          size
	  offset
          (translate-protection-flags protection)
          (translate-mmap-access-flags protection mmap-access)))

(defun mmap (path &key (open '(:read))
		     (protection '(:read))
		     (mmap-access '(:private))
		     size
		    (offset 0))
  (mmap% (open-file path :open open)
         :size size
	 :offset offset
         :protection protection
	 :mmap-access mmap-access))

(defun munmap (addr fd size)
  (declare (ignore size))
  (error-unless (unmap-view-of-file addr))
  (when fd
    (destructuring-bind (fd . handle) fd
      (error-unless (close-handle handle))
      (error-unless (close-handle fd))))
  NIL)

(defun msync (addr fd size &key (flags '(:sync)))
  (error-unless (flush-view-of-file addr size))
  (when (find :sync flags)
    (error-unless (flush-file-buffers (car fd))))
  NIL)

(defun mprotect (addr size protection)
  (cffi:with-foreign-object (oldprotect 'dword)
    (error-unless (virtual-protect addr size
				   (translate-protection-flags protection)
				   oldprotect))
    NIL))
