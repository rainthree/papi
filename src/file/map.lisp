(uiop:define-package #:papi/file/map
    (:use :cl)
  (:import-from #:papi/common :*this-pathname* :package+= :deft)
  (:import-from #:40ants-doc :defsection)
  (:use-reexport
   #+unix #:papi/file/map/unix
   #+windows #:papi/file/map/windows)
  (:export #:with-mmap))

(in-package #:papi/file/map)

(defsection @doc (:title (package+= " documentation") :ignore-words ()
		  :export nil)
  ((with-mmap) macro)
  (@examples section)
  (@os-specific-doc section))

(defsection @examples (:title (package+= " examples")
		       :export nil)
  (:with-mmap function))

(defmacro with-mmap ((addr fd-and-handle size path &rest args) &body body)
  "on Windows both the file handle and the mapping object handle are returned as a cons in fd-and-handle"
  (let ((addrg (gensym "ADDR"))
        (fd-and-handleg   (gensym "FD-AND-HANDLE"))
        (sizeg (gensym "SIZE")))
    `(multiple-value-bind (,addrg ,fd-and-handleg ,sizeg) (mmap ,path ,@args)
       (unwind-protect
            (let ((,addr ,addrg)
                  (,fd-and-handle ,fd-and-handleg)
                  (,size ,sizeg))
              (declare (ignorable ,fd-and-handle ,size))
              ,@body)
         (munmap ,addrg ,fd-and-handleg ,sizeg)))))

(defparameter *p* #.*this-pathname*)

(deft :with-mmap (:doc "Shows how to use with-mmap:")
    (let (mmapped read)    
      (setf read (alexandria:read-file-into-string *p* :external-format :utf-8))   
      (with-mmap (addr fd-and-handle size *p*)
	(setf mmapped (cffi:foreign-string-to-lisp addr :count size :encoding :utf-8)))
      (string= read mmapped))
    t)
