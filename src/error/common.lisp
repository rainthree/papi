(uiop:define-package #:papi/error/common
    (:use :cl #:40ants-doc)
  (:import-from #:papi/common :package+= :deft)
  (:export #:api-error
	   #:code-of
	   #:message-of
	   #:report-fn-of)
  (:documentation "Error handing APIs common definitions (describe papi/error/common::@doc)"))

(in-package #:papi/error/common)

(defsection @doc (:title (package+= " documentation")
		  :export nil)
  ((api-error) function)
  (api-error condition)
  (*default-api-report-fn* variable)
  ((code-of message-of report-fn-of) (reader api-error))
  (@examples section))

(defvar *default-api-report-fn*
  (lambda (c s)
    (format s "API error (code ~d):~%  ~a"
            (code-of c) (message-of c))))

(define-condition api-error (simple-error)
  ((%code      :initarg :code      :reader code-of)
   (%message   :initarg :message   :reader message-of)
   (%report-fn :initarg :report-fn :reader report-fn-of
	       :initform *default-api-report-fn*))
  (:report (lambda (c s)
	     (funcall (report-fn-of c) c s))))

(defun api-error (code message &key (report-fn *default-api-report-fn*))
  (error 'api-error :code code :message message :report-fn report-fn))
 
(defsection @examples (:title (package+= " examples")
		       :export nil)
  (:signal-api-error function))

(deft :signal-api-error (:doc "Shows how to signal api-error")
  (handler-case
      (api-error -1 "My Error Message")
    (api-error (c) t))
  t)
