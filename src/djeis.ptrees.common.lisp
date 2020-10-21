(defpackage #:djeis.ptrees.common.impl
  (:use #:cl #:djeis.ptrees.common)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum)))

(in-package #:djeis.ptrees.common.impl)

(defstruct box val)
