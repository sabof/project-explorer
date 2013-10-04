(ert-deftest tf/compress-tree ()
  (let (( tree '("path0"
                 ("path1"
                  ;; Reverse ATM
                  "somefile2.js"
                  "somefile.js"
                  ))))
    (should (tree-equal (tf/compress-tree tree)
                        '("path0/path1"
                          ;; Reverse ATM
                          "somefile2.js"
                          "somefile.js")
                        :test 'string-equal))))

(ert-deftest tf/sort ()
  (let* (( alist
           '("root"
             "file"
             ("node2")
             ("node1")
             ("node3"
              ("subnode1"
               "subnode3"
               "subnode2"))
             ))
         ( result (tf/sort (copy-tree alist))))
    (should (tree-equal result
                        '("root"
                          ("node1")
                          ("node2")
                          ("node3"
                           ("subnode1"
                            "subnode2"
                            "subnode3"))
                          "file")
                        :test 'equal))
    ))

(ert-deftest tf/print-indented-tree ()
  (let* (( tree '("root"
                  ("node2")
                  ("node1")
                  ("node3"
                   ("subnode1"
                    ("subnode3")
                    ("subnode2")))))
         ( expected-result
           "node2/
node1/
node3/
\tsubnode1/
\t\tsubnode3/
\t\tsubnode2/
")
         ( result
           (with-temp-buffer
             (tf/print-indented-tree tree)
             (buffer-string))))
    (should (string-equal expected-result
                          result))))

(ert-deftest tf/path-to-list ()
  (let* ((input "/a/b/c/d")
         (output (tf/path-to-list input))
         (expected-output (list "a/" "b/" "c/" "d")))
    (should (equal output expected-output)))
  (let* ((input "/a/b/c/d/")
         (output (tf/path-to-list input))
         (expected-output (list "a/" "b/" "c/" "d/")))
    (should (equal output expected-output))))
