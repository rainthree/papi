(uiop:define-package #:papi/error/windows
    (:use :cl #:40ants-doc)
  (:import-from #:papi/common :package+= :deft)
  (:import-from #:papi/windows :wchar_t :dword)
  (:import-from #:papi/error/common :api-error)
  (:export
   :+format-message-ignore-inserts+
   :+format-message-from-system+
   :get-last-error
   :format-message
   :error-unless
   :error-when
	   
   :@os-specific-doc))

(in-package #:papi/error/windows)

(defsection @os-specific-doc (:title (package+= " documentation")
			      :export nil)
  ((+format-message-ignore-inserts+
    +format-message-from-system+) constant)
  ((get-last-error
   format-message
   error-unless
   error-when) function)
  (@examples section))

(defconstant +format-message-from-system+ 4096)
(defconstant +format-message-ignore-inserts+ 512)

(cffi:defcfun (get-last-error "GetLastError") dword)

(cffi:defcfun (format-message "FormatMessageW") dword
  (flags dword)
  (source :pointer)
  (message-id dword)
  (language-id dword)
  (buffer :pointer)
  (size dword)
  (arguments :pointer))

(defun error% ()
  "Signals papi/error/common:api-error with the message from get-last-error" 
  (let ((errno (get-last-error)))
      (cffi:with-foreign-object (string 'wchar_t 256)
        (format-message (logior +format-message-from-system+ +format-message-ignore-inserts+)
                        (cffi:null-pointer) errno 0 string 256 (cffi:null-pointer))
        (api-error errno (cffi:foreign-string-to-lisp string :encoding :utf-16le)))))

(defun error-unless (result)
  (unless result
    (error%)))

(defun error-when (result)
  (when result
    (error%)))

(defsection @examples (:title (package+= " examples"))
  (:error% function))

(deft :error% (:doc "Shows how error-when, error-unless and (error%) can be used. ")
  (values
     (handler-case
	(error-when (quote call-some-api))
       (api-error () 1))
     (handler-case
	(error-unless nil)
       (api-error () 2))
     (handler-case
	(error%)
       (api-error () 3)))
  1 2 3)
