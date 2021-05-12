(asdf:defsystem fipl
  :name "fipl"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "a Forth in Lisp"
  :licence "MIT"
  :build-operation "asdf:program-op"
  :build-pathname "fipl"
  :entry-point "fipl:repl"
  :depends-on ()
  :serial t
  :components ((:file "fipl") (:file "tests")))
