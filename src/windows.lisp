;;;; windows common symbols that can be imported by various submodules
(uiop:define-package #:papi/windows
    (:use #:cl)
  (:import-from #:cffi)
  (:export #:lpsecurity-attributes
	   #:wchar_t #:handle #:word #:dword #:size_t #:large-integer))

(in-package #:papi/windows)

(cffi:defctype lpsecurity-attributes :pointer) ; used by CreateFile, CreatePipe, CreateProcess, RegCreateKeyEx, RegSaveKeyEx, etc.

(cffi:defctype wchar_t :uint16) ; in the Microsoft compiler, it represents a 16-bit wide character used to store Unicode encoded as UTF-16LE
(cffi:defctype handle :pointer)
(cffi:defctype word :unsigned-short)
(cffi:defctype dword :unsigned-long)

(cffi:defctype size_t  #+x86-64 :uint64 #+x86 :uint32)
(cffi:defctype large-integer :uint64) ; used by get-file-size
