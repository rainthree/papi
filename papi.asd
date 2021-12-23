(asdf:defsystem papi
  :version "0.0.1"
  :license "Public Domain"
  :description "Portability layer for APIs across different operating systems"
  :source-control (:git "https://github.com/rainthree/papi.git")
  :class :package-inferred-system
  :depends-on (:papi/all)
  :defsystem-depends-on ("rt" "cffi")
  :pathname "src"
  ;; :in-order-to ((test-op (load-op :papi)))
  :perform (test-op (o s)
	     (load (asdf:system-relative-pathname s "src/test.lisp"))))
