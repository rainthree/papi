(uiop:define-package #:papi/file/all
    (:shadow #:@os-specific-doc)
    (:use-reexport #:papi/file     ; general apis for opening/closing files
		   #:papi/file/map ; files memory mapping apis 
		   ))
