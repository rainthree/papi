(uiop:define-package #:papi/common
    (:use #:cl #:40ants-doc)
  (:import-from :rt)
  (:export 
   #:translate-path
   #:*this-pathname*
   #:+=
   #:package+=
   #:deft)
  (:documentation "General common utilities for all operating systems modules. (describe papi/common::@doc)"))

(in-package #:papi/common)

(defmethod documentation ((symbol symbol) (kind (eql :symbol-macro))) (getf symbol 'symbol-macro-documentation))
(defmethod (setf documentation) (new-doc (symbol symbol) (kind (eql :symbol-macro))) (setf (getf symbol 'symbol-macro-documentation) new-doc))
(defmethod documentation ((symbol symbol) (kind (eql :symbol-macro))) (get symbol 'symbol-macro-documentation))
(defmethod (setf documentation) (new-doc (symbol symbol) (kind (eql :symbol-macro))) (setf (get symbol 'symbol-macro-documentation) new-doc))
(setf (documentation 'foo :symbol-macro) "bla bla")
;;; (documentation 'foo :symbol-macro) #| --> "bla bla" |#
;;; <pjb> of course, you have to choose where to store that documentation.  Also, when and how to store it and retrieve it from a fasl file.

(defsection @doc (:title "PAPI/COMMON documentation")
  ((translate-path += package+=) function)
  (deft macro)
  (*this-pathname* symbol-macro))
      
(defun translate-path (path)
  "Translates path to a namestring if it is not a string already."
  (etypecase path
    (string path)
    (pathname (uiop:native-namestring path))))

(define-symbol-macro *this-pathname*
    (or *compile-file-pathname* *load-pathname*
	(error "COMPILE-FILE or LOAD this file.")))

(defun *this-pathname* ()
  12)

(setf (documentation '*this-pathname* :symbol-macro)
      "expands to code that returns the name of the current sourcefile that is being loaded or compiled. Usually used with #. (sharp-dot)")


(eval-when (:compile-toplevel :execute :load-toplevel)  
  (defun += (&rest params &aux (first (car params))
			    (rest (cdr params)))
    "Coerces the parameters to strings and concatenates them." 
    (declare (optimize (speed 0) (debug 3)))
    (when (null first)
      (setf first ""))
    (if (null rest)
	first
	(if (and (not (stringp first))
		 (vectorp first))
	    (concatenate 'vector
			 first
			 (apply #'+= rest))
	    (format nil "~{~A~}" (mapcar (lambda(p) (if p p ""))
					 params)))))
  (defun ~% (&optional (n 1))
    (with-output-to-string (os)
      (dotimes (i n)
	(format os "~%"))))
  (defmacro deft (name (&key doc) code &rest values)
    "Used to define tests which are also documented examples.
     Defines a test using rt:deftest (and it is better than it because it also accepts a docstring)
     Defines a function with the same name as the test name and which has the same docstring as the test but with the sourcecode of the test appended."
    `(progn
       (defun ,name ()
	 ,(+=
	   (if (null doc)
	       ""
	       (+= doc (~% 2)))
	   (+= "```lisp" (~%)
	       code (~%)
	       ";=> " (if (> (length values) 1)
			  values
			  (first values))
	       (~%)
	       "```" (~%)))
	 (rt:do-test ,name))
       (rt:deftest ,name ,code ,@values))))

(deft :+= (:doc "Shows how to concatenate strings using +=")
    (values
     (equalp #(2 4) (+= #(2) #(4)))
     (equalp (+= "a" 2) "a2")
     (equalp (+= "a" nil) "a")
     (equalp (+= ) "")
     (equalp (+= nil "works") "works"))
  t t t t t)

(defun package+= (string &optional (package *package*))
  "Concatenates the name of package (default *package*) with the parameter."
  (+= (package-name package) string))
