
(defpackage #:djeis.ptrees.hamt
  (:use #:cl)
  (:import-from #:djeis.ptrees.common
                #:make-box #:box-val)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum))
  (:export #:make-ptrie #:insert! #:delete!
           #:transient-for #:persistent!
           #:lookup))

(in-package #:djeis.ptrees.hamt)

(defstruct hamt
  root 
  test
  hash)

(defstruct (phamt (:include hamt))
  transient-for)

(defstruct (thamt (:include hamt))
  transient-box
  persistent!)

(defstruct node
  transient-box)

(defstruct (array-node (:include node))
  (databits 0)
  (nodebits 0)
  (array (make-array 0)))


(defun infect-node-keep-array (box node)
  (if (eq (node-transient-box node) box)
      node
      (s:lret ((node (copy-array-node node)))
        (setf (node-transient-box node) box))))

(defun infect-node-copy-array (box node)
  (if (eq (node-transient-box node) box)
      node
      (let* ((node (copy-array-node node))
             (array (a:copy-array (array-node-array node))))
        (setf (node-transient-box node) box
              (array-node-array node) array)
        (values node array))))

(defun count-n-bits (n bits)
  (logcount (ldb (byte n 0) bits)))

(defmacro idx-switch ((node level hash) &body (data-case node-case neither-case))
  (a:once-only (node level hash)
    (a:with-gensyms (nodebits-sym databits-sym array-sym code-sym)
      (flet ((process-case (the-case)
               (destructuring-bind (&key didx nidx code array nodebits databits) (first the-case)
                 `(let (,@(when didx `((,didx (* 2 (count-n-bits ,code-sym ,databits-sym)))))
                        ,@(when nidx `((,nidx (- (length ,array-sym) (count-n-bits ,code-sym ,nodebits-sym) 1))))
                        ,@(when code `((,code ,code-sym)))
                        ,@(when nodebits `((,nodebits ,nodebits-sym)))
                        ,@(when databits `((,databits ,databits-sym)))
                        ,@(when array `((,array ,array-sym))))
                    ,@(rest the-case)))))
        `(let ((,nodebits-sym (array-node-nodebits ,node))
               (,databits-sym (array-node-databits ,node))
               (,array-sym (array-node-array ,node))
               (,code-sym (ldb (byte 5 (* 5 ,level)) ,hash)))
           (cond ((logbitp ,code-sym ,databits-sym)
                  ,(process-case data-case))
                 ((logbitp ,code-sym ,nodebits-sym)
                  ,(process-case node-case))
                 (t ,(process-case neither-case))))))))

(defun lookup (trie key)
  (let ((test (hamt-test trie))
        (hasher (hamt-hash trie)))
    (labels ((node-lookup (node hash key level)
               (typecase node
                 (array-node (array-node-lookup node hash key level))
                 (list (a:if-let ((c (assoc key node :test test)))
                         (values (cdr c) t)
                         (values nil nil)))))
             (array-node-lookup (node hash key level)
               (idx-switch (node level hash)
                 ((:didx idx :array array)
                  (if (funcall test (aref array idx) key)
                      (values (aref array (1+ idx)) t)
                      (values nil nil)))
                 ((:nidx idx :array array)
                  (node-lookup (aref array idx) hash key (1+ level)))
                 (() (values nil nil)))))
      (node-lookup (hamt-root trie) (funcall hasher key) key 0))))

(defun insert! (trie key value)
  (check-type trie thamt)
  (assert (eq (bt:current-thread) (box-val (thamt-transient-box trie))))
  (let ((test (hamt-test trie))
        (hasher (hamt-hash trie))
        (box (thamt-transient-box trie)))
    (labels ((node-insert (node hash key value level)
               (typecase node
                 (array-node (array-node-insert node hash key value level))
                 (list (acons key value (remove key node :test test :key #'car)))))
             (make-node (level) (if (< level 5) (make-array-node :transient-box box) '()))
             (array-node-insert (node hash key value level)
               (idx-switch (node level hash)
                 ((:didx idx :code code :nodebits nodebits :array array)
                  (let* ((ekey (aref array idx)))
                    (if (funcall test ekey key)
                        (s:lret ((node (infect-node-copy-array box node)))
                          (setf (aref (array-node-array node) (1+ idx)) value))
                        (s:lret* ((new-child-node (make-node (1+ level)))
                                  (new-array (make-array (1- (length array))))
                                  (node-idx (- (length array) (count-n-bits code nodebits) 2))
                                  (node (infect-node-keep-array box node)))
                          (node-insert new-child-node
                                       hash key value
                                       (1+ level))
                          (node-insert new-child-node
                                       (funcall hasher ekey)
                                       ekey (aref array (1+ idx))
                                       (1+ level))
                          (replace new-array array :end1 idx)
                          (replace new-array array :start1 idx :start2 (+ 2 idx) :end1 node-idx)
                          (replace new-array array :start1 (+ 1 node-idx) :start2 (+ 2 node-idx))
                          (setf (aref new-array node-idx) new-child-node
                                (array-node-array node) new-array
                                (logbitp code (array-node-databits node)) nil
                                (logbitp code (array-node-nodebits node)) t)))))
                 ((:nidx idx :array array)
                  (s:lret* ((new-child-node (node-insert (aref array idx) hash key value (1+ level)))
                            (node (infect-node-copy-array box node)))
                    (setf (aref (array-node-array node) idx) new-child-node)))
                 ((:didx idx :code code :array array)
                  (s:lret* ((new-array (make-array (+ 2 (length array))))
                            (node (infect-node-keep-array box node)))
                    (replace new-array array :end1 idx)
                    (replace new-array array :start1 (+ 2 idx) :start2 idx)
                    (setf (aref new-array idx) key
                          (aref new-array (+ 1 idx)) value
                          (logbitp code (array-node-databits node)) t
                          (array-node-array node) new-array))))))
      (s:callf #'node-insert (hamt-root trie) (funcall hasher key) key value 0)
      trie)))

(defun delete! (trie key)
  (check-type trie thamt)
  (assert (eq (bt:current-thread) (box-val (thamt-transient-box trie))))
  (let ((test (hamt-test trie))
        (hasher (hamt-hash trie))
        (box (thamt-transient-box trie)))
    (labels ((node-delete (node hash key level)
               (typecase node
                 (array-node (array-node-delete node hash key level))
                 (list (remove key node :test test :key #'car))))
             (array-node-delete (node hash key level)
               (idx-switch (node level hash)
                 ((:didx idx :code code :array array)
                  (let* ((ekey (aref array idx)))
                    (if (funcall test ekey key)
                        (s:lret ((new-array (make-array (- (length array) 2)))
                                 (node (infect-node-keep-array box node)))
                          (replace new-array array :end1 idx)
                          (replace new-array array :start1 idx :start2 (+ 2 idx))
                          (setf (array-node-array node) new-array
                                (logbitp code (array-node-databits node)) nil))
                        node)))
                 ((:nidx idx :array array)
                  (s:lret* ((new-child-node (node-delete (aref array idx) hash key (1+ level)))
                            (node (infect-node-copy-array box node)))
                    (setf (aref (array-node-array node) idx) new-child-node)))
                 (() node))))
      (s:callf #'node-delete (hamt-root trie) (funcall hasher key) key 0)
      trie)))

(defun transient-for (trie)
  (check-type trie phamt)
  (funcall (phamt-transient-for trie) trie))

(defun persistent! (trie)
  (check-type trie thamt)
  (assert (eq (bt:current-thread) (box-val (thamt-transient-box trie))))
  (funcall (thamt-persistent! trie) trie))

(defun make-ptrie (&optional (test #'equal) (hash #'sxhash))
  (labels ((transient-for (trie)
             (make-thamt :transient-box (make-box :val (bt:current-thread))
                         :root (hamt-root trie)
                         :test (hamt-test trie)
                         :hash (hamt-hash trie)
                         :persistent! #'persistent!))
           (persistent! (trie)
             (setf (box-val (thamt-transient-box trie)) nil)
             (make-phamt :root (hamt-root trie)
                         :test (hamt-test trie)
                         :hash (hamt-hash trie)
                         :transient-for #'transient-for)))
    (make-phamt :root (make-array-node) :test test :hash hash :transient-for #'transient-for)))
