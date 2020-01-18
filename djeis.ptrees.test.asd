(asdf:defsystem #:djeis.ptrees.test
  :description "Tests for djeis.ptrees."
  :author "Elijah Malaby <emalaby@mail.usf.edu>"
  :maintainer "Elijah Malaby <emalaby@mail.usf.edu>"
  :license "MIT"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:djeis.ptrees #:prove)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (o c) (uiop:symbol-call '#:prove '#:run c))
  :components
  ((:test-file "bst")))
