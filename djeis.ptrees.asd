;;;; djeis.ptrees.asd

(asdf:defsystem #:djeis.ptrees
  :description "Describe djeis.ptrees here"
  :author "Elijah Malaby <emalaby@mail.usf.edu>"
  :maintainer "Elijah Malaby <emalaby@mail.usf.edu>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:alexandria #:serapeum #:bordeaux-threads)
  :in-order-to ((asdf:test-op (asdf:test-op #:djeis.ptrees.test)))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "djeis.ptrees.common")
               (:file "djeis.ptrees.bst")
               (:file "djeis.ptrees.hamt")))
