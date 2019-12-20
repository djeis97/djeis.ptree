

(defpackage #:djeis.ptrees.bst
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria) (#:s #:serapeum))
  (:export #:lookup #:insert! #:delete! #:make-wb-tree
           #:transient-for #:persistent!))

(in-package #:djeis.ptrees.bst)

(defstruct box val)

(defstruct base-tree
  root
  cmp)

(defstruct (ptree (:include base-tree))
  transient-for)

(defstruct (ttree (:include base-tree))
  transient-box
  persistent!
  joiner)

(defstruct node
  transient-box
  key
  value
  left
  right)

(defun transient-for (tree)
  (funcall (ptree-transient-for tree) tree))
(defun persistent! (tree)
  (funcall (ttree-persistent! tree) tree))

(defun lookup (tree key)
  (let ((cmpfn (base-tree-cmp tree)))
    (labels ((recursor (node)
               (if node
                   (let ((nkey (node-key node)))
                     (cond
                       ((funcall cmpfn key nkey)
                        (recursor (node-left node)))
                       ((funcall cmpfn nkey key)
                        (recursor (node-right node)))
                       (t (values (node-value node) t))))
                   (values nil nil))))
      (recursor (base-tree-root tree)))))

(defun %split! (tree node key)
  (let* ((joiner (ttree-joiner tree))
         (cmp (base-tree-cmp tree)))
    (when (null node)
      (return-from %split!
        (values nil nil nil)))
    (let* ((node-true-key (node-key node)))
      (cond
        ((funcall cmp key node-true-key)
         (multiple-value-bind (l returned-node r)
             (%split! tree (node-left node) key)
           (values l returned-node (funcall joiner tree
                                            r (node-key node) (node-value node) (node-right node)
                                            node))))
        ((funcall cmp node-true-key key)
         (multiple-value-bind (l returned-node r)
             (%split! tree (node-right node) key)
           (values (funcall joiner tree (node-left node) (node-key node) (node-value node) l node)
                   returned-node
                   r)))
        (t (values (node-left node) node (node-right node)))))))

(defun %split-last! (tree node)
  (if (node-right node)
      (multiple-value-bind (split-tree split-node)
          (%split-last! tree (node-right node))
        (values (funcall (ttree-joiner tree) tree
                         (node-left node) (node-key node) (node-value node) split-tree
                         node)
                split-node))
      (values (node-left node) node)))

(defun %join2! (tree left right)
  (if left
      (multiple-value-bind (split-left split-node)
          (%split-last! tree left)
        (funcall (ttree-joiner tree) tree
                 split-left (node-key split-node) (node-value split-node) right
                 split-node))
      right))

(defun insert! (tree key value)
  (check-type tree ttree)
  (assert (eq (bt:current-thread) (box-val (ttree-transient-box tree))))
  (let* ((cmp (base-tree-cmp tree))
         (joiner (ttree-joiner tree)))
    (labels ((recursor (node)
               (if node
                   (let ((node-key (node-key node)))
                     (cond
                       ((funcall cmp key node-key)
                        (funcall joiner tree
                                 (recursor (node-left node)) (node-key node) (node-value node) (node-right node)
                                 node))
                       ((funcall cmp node-key key)
                        (funcall joiner tree
                                 (node-left node) (node-key node) (node-value node) (recursor (node-right node))
                                 node))
                       (t (funcall joiner tree (node-left node) key value (node-right node) node))))
                   (funcall joiner tree nil key value nil))))
      (s:callf #'recursor (base-tree-root tree))
      tree)))

(defun delete! (tree key)
  (check-type tree ttree)
  (assert (eq (bt:current-thread) (box-val (ttree-transient-box tree))))
  (let* ((cmp (base-tree-cmp tree))
         (joiner (ttree-joiner tree)))
    (labels ((recursor (node)
               (if node
                   (let ((node-key (node-key node)))
                     (cond
                       ((funcall cmp key node-key)
                        (funcall joiner tree
                                 (recursor (node-left node)) (node-key node) (node-value node) (node-right node)
                                 node))
                       ((funcall cmp node-key key)
                        (funcall joiner tree
                                 (node-left node) (node-key node) (node-value node) (recursor (node-right node))
                                 node))
                       (t (%join2! tree (node-left node) (node-right node)))))
                   nil)))
      (s:callf #'recursor (base-tree-root tree))
      tree)))


(defun ub-join (tree left key value right &optional old-node)
  (let ((box (ttree-transient-box tree)))
    (unless (and old-node (eql (node-transient-box old-node) box))
      (setf old-node (make-node :transient-box box))))
  (setf (node-left old-node) left
        (node-key old-node) key
        (node-right old-node) right
        (node-value old-node) value)
  old-node)

(defun make-ub-tree (&optional (cmp #'<))
  (labels ((transient-for (tree)
             (make-ttree :transient-box (make-box :val (bt:current-thread))
                         :root (base-tree-root tree)
                         :cmp (base-tree-cmp tree)
                         :persistent! #'persistent!
                         :joiner #'ub-join))
           (persistent! (tree)
             (setf (box-val (ttree-transient-box tree)) nil)
             (make-ptree :root (base-tree-root tree)
                         :cmp (base-tree-cmp tree)
                         :transient-for #'transient-for)))
    (make-ptree :cmp cmp :transient-for #'transient-for)))


(defstruct (wbnode (:include node))
  size)

(defun wb-join (tree left key value right &optional old-node)
  (let ((box (ttree-transient-box tree)))
    (labels ((get-node (node)
               (cond ((null node) (make-wbnode :transient-box box))
                     ((eql (node-transient-box node) box) node)
                     (t (s:lret ((node (copy-wbnode node)))
                          (setf (node-transient-box node) box))))))
      (s:callf #'get-node old-node)
      (labels ((size (node)
                 (if node
                     (wbnode-size node)
                     0))
               (weight (node)
                 (1+ (size node))) 
               (balanced (a b) (<= 0.29 (/ (+ 1 a) (+ 2 a b)) (- 1 0.29)))
               (heavy (a b) (and a (or (not b) (> (weight a) (weight b)))))
               (rotate-left (node)
                 (s:callf #'get-node node)
                 (s:callf #'get-node (node-right node))
                 (rotatef (node-right node) node (node-left (node-right node)))
                 node)
               (rotate-right (node)
                 (s:callf #'get-node node)
                 (s:callf #'get-node (node-left node))
                 (rotatef (node-left node) node (node-right (node-left node)))
                 node)
               (join-right (left right)
                 (if (balanced (size left) (size right))
                     (progn
                       (setf (node-left old-node) left
                             (node-right old-node) right
                             (wbnode-size old-node) (+ 1
                                                       (size left)
                                                       (size right))
                             (node-key old-node) key
                             (node-value old-node) value)
                       old-node)
                     (let ((rec (join-right (node-right left) right)))
                       (s:callf #'get-node left)
                       (cond
                         ((balanced (size (node-left left)) (size rec))
                          (setf (node-right left) rec
                                (wbnode-size left) (+ 1
                                                      (size (node-left left))
                                                      (size rec)))
                          left)
                         ((and (balanced (size (node-left left)) (size (node-left rec)))
                               (balanced (+ (size (node-left left)) (size (node-left rec)))
                                         (size (node-right rec))))
                          (setf (node-right left) rec
                                (wbnode-size left) (+ 1
                                                      (size (node-left left))
                                                      (size rec)))
                          (rotate-left left))
                         (t
                          (s:callf #'rotate-right rec)
                          (setf (node-right left) rec
                                (wbnode-size left) (+ 1
                                                      (size (node-left left))
                                                      (size rec)))
                          (rotate-left left))))))
               (join-left (left right)
                 (if (balanced (size left) (size right))
                     (progn
                       (setf (node-left old-node) left
                             (node-right old-node) right
                             (wbnode-size old-node) (+ 1
                                                       (size left)
                                                       (size right))
                             (node-key old-node) key
                             (node-value old-node) value)
                       old-node)
                     (let ((rec (join-left left (node-left right))))
                       (s:callf #'get-node right)
                       (cond
                         ((balanced (size (node-right right)) (size rec))
                          (setf (node-left right) rec
                                (wbnode-size right) (+ 1
                                                       (size (node-right right))
                                                       (size rec)))
                          right)
                         ((and (balanced (size (node-right right)) (size (node-right rec)))
                               (balanced (+ (size (node-right right)) (size (node-right rec)))
                                         (size (node-left rec))))
                          (setf (node-left right) rec
                                (wbnode-size right) (+ 1
                                                       (size (node-right right))
                                                       (size rec)))
                          (rotate-right right))
                         (t
                          (s:callf #'rotate-left rec)
                          (setf (node-left right) rec
                                (wbnode-size right) (+ 1
                                                       (size (node-right right))
                                                       (size rec)))
                          (rotate-right right)))))))
        (cond ((heavy left right) (join-right left right))
              ((heavy right left) (join-left left right))
              (t (setf (node-left old-node) left
                       (node-right old-node) right
                       (wbnode-size old-node) (+ 1
                                                 (size left)
                                                 (size right))
                       (node-key old-node) key
                       (node-value old-node) value)
                 old-node))))))


(defun make-wb-tree (&key (cmp #'<))
  (labels ((transient-for (tree)
             (make-ttree :transient-box (make-box :val (bt:current-thread))
                         :root (base-tree-root tree)
                         :cmp (base-tree-cmp tree)
                         :persistent! #'persistent!
                         :joiner #'wb-join))
           (persistent! (tree)
             (setf (box-val (ttree-transient-box tree)) nil)
             (make-ptree :root (base-tree-root tree)
                         :cmp (base-tree-cmp tree)
                         :transient-for #'transient-for)))
    (make-ptree :cmp cmp :transient-for #'transient-for)))
