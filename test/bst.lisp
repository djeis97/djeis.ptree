(defpackage djeis.ptrees.test.bst
  (:use #:cl #:prove)
  (:local-nicknames (#:bst #:djeis.ptrees.bst)
                    (#:s #:serapeum)))

(in-package :djeis.ptrees.test.bst)

(defun unbalanced-tree-tests ()
  (subtest "Unbalanced trees, basic API"
    (plan 8)
    (let* ((tree (s:~> (bst::make-ub-tree)
                       (bst:transient-for)
                       (bst:insert! 5 "foo")
                       (bst:insert! 6 "bar")
                       (bst:insert! 4 "baz")
                       (bst:insert! 3 nil)
                       (bst:persistent!)))
           (v2 (s:~> tree
                     (bst:transient-for)
                     (bst:insert! 6 "quux")
                     (bst:delete! 5)
                     (bst:delete! 3)
                     (bst:persistent!))))
      (is (bst:lookup tree 5) "foo")
      (is (bst:lookup tree 6) "bar")
      (is (bst:lookup tree 4) "baz")
      (is-values (bst:lookup tree 3) '(nil t))
      (is-values (bst:lookup tree 2) '(nil nil))
      (is (bst:lookup v2 6) "quux")
      (is-values (bst:lookup v2 5) '(nil nil))
      (is-values (bst:lookup v2 3) '(nil nil)))
    (finalize)))

(plan 1)

(unbalanced-tree-tests)

(finalize)
