(ert-deftest tf/paths-to-tree ()
  (let (( sample-paths
          (list "./path1/somefile.js"
                "./path1/somefile2.js")))
    (should (tree-equal (tf/paths-to-tree sample-paths)
                        '("./"
                          ("path1/"
                           ;; Reverse ATM
                           ("somefile2.js")
                           ("somefile.js")))
                        :test 'string-equal))))

(ert-deftest tf/compress-tree ()
  (let (( tree '("path0/"
                 ("path1/"
                  ;; Reverse ATM
                  ("somefile2.js")
                  ("somefile.js")
                  ))))
    (should (tree-equal (tf/compress-tree tree)
                        '("path0/path1/"
                          ;; Reverse ATM
                          ("somefile2.js")
                          ("somefile.js"))
                        :test 'string-equal))))

;; (ert-deftest tf/tree-mark-folders ()
;;   (let (( tree (list "."
;;                      (list "path1"
;;                            ;; Reverse ATM
;;                            (list "somefile2.js")
;;                            (list "somefile.js")
;;                            ))))
;;     (should (tree-equal (tf/tree-mark-folders tree)
;;                         '("./"
;;                           ("path1/"
;;                            ;; Reverse ATM
;;                            ("somefile2.js")
;;                            ("somefile.js")
;;                            ))
;;                         :test 'string-equal))))

(ert-deftest tf/sort ()
  (let* (( alist
           '("root/"
             ("node2")
             ("node1")
             ("node3/"
              ("subnode1/"
               ("subnode3")
               ("subnode2")))))
         ( result (tf/sort (copy-tree alist))))
    (should (= 4 (length result)))
    (should (= 2 (length (cadr result))))
    (should (= 3 (length (cadadr result))))
    (should (tree-equal result
                        '("root/"
                          ("node3/"
                           ("subnode1/"
                            ("subnode2")
                            ("subnode3")))
                          ("node1")
                          ("node2"))
                        :test 'equal))
    ))

(ert-deftest tf/get-untraversed-nodes ()
  (let* (( tree '("root"
                  ("node2/")
                  ("node1")
                  ("node3"
                   ("subnode1"
                    ("subnode3/")
                    ("subnode2")))))
         ( expected-result '("subnode3/" "node2/"))
         ( result (tf/get-untraversed-nodes tree)))
    (should (= (length expected-result)
               (length result)
               ))
    (should (= (length expected-result)
               (length (cl-intersection result
                                        expected-result
                                        :test 'string-equal))))))

(ert-deftest tf/path-to-list ()
  (let* ((input "/a/b/c/d")
         (output (tf/path-to-list input))
         (expected-output (list "a/" "b/" "c/" "d")))
    (should (equal output expected-output)))
  (let* ((input "/a/b/c/d/")
         (output (tf/path-to-list input))
         (expected-output (list "a/" "b/" "c/" "d/")))
    (should (equal output expected-output))))

(ert-deftest tf/directory-branch-p ()
  (should (tf/directory-branch-p '("abc/")))
  (should-not (tf/directory-branch-p '("abc"))))
