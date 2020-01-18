(defpackage #:djeis.ptrees.common
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum))
  (:export make-box box-val))

(in-package #:djeis.ptrees.common)

(defstruct box val)
