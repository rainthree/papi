(uiop:define-package #:papi/file
    (:use :cl #:40ants-doc)
  (:import-from #:papi/common :*this-pathname* :package+= :deft)
  (:use-reexport
   #+unix :papi/file/unix
   #+windows :papi/file/windows)
  (:documentation "File APIs (describe papi/file::@doc)"))

(in-package #:papi/file)

(defsection @doc (:title (package+= " documentation") :ignore-words ()
		  :export nil)
  "Functions"
  ((open-file close-file get-file-size) function)
  (@examples section)
  (@os-specific-doc section))

(defsection @examples (:title (package+= " examples")
		       :export nil)
  "Examples which can be executed, for example (:file_open&close&get-size)"
  (:file_open&close&get-size function))

(defparameter *p* #.*this-pathname*)

(deft :file_open&close&get-size
    (:doc "Example that shows how to use open-file get-file-size and close-file")
    (let ((p *p*))
      (when (probe-file p)
	(let ((fd (open-file p)))
	  (assert (> (get-file-size fd) 0))
	  (close-file fd))))
  t)
