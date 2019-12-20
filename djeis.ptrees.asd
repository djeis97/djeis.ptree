;;;; djeis.ptrees.asd

(asdf:defsystem #:djeis.ptrees
  :description "Describe djeis.ptrees here"
  :author "Elijah Malaby <emalaby@mail.usf.edu>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:serapeum #:bordeaux-threads)
  :components ((:file "djeis.ptrees.common")
               (:file "djeis.ptrees.bst")))
