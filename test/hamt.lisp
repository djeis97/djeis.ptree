(defpackage djeis.ptrees.test.hamt
  (:use #:cl #:prove)
  (:local-nicknames (#:hamt #:djeis.ptrees.hamt)
                    (#:a #:alexandria)
                    (#:s #:serapeum)))

(in-package :djeis.ptrees.test.hamt)

(defun my-hash (key)
  (a:switch (key :test #'equal)
    ("A" #b000001)
    ("B" #b100001)
    ("C" #b000000)
    ("D" #b000011)))

(defun trie-tests ()
  (subtest "Basic trie tests"
    (plan 3)
    (let* ((trie (hamt:make-ptrie #'equal #'my-hash))
           (trie1 (s:~> trie
                        (hamt:transient-for)
                        (hamt:insert! "A" "A")
                        (hamt:persistent!)))
           (trie2 (s:~> trie
                        (hamt:transient-for)
                        (hamt:insert! "B" "B"))))
      (is-values (hamt:lookup trie1 "A") '("A" t))
      (is-values (hamt:lookup trie1 "B") '(nil nil))
      (is (hamt:lookup trie2 "B") "B"))
    (finalize)))

(plan 1)

(trie-tests)

(finalize)
