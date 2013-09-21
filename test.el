(ert-deftest paths-to-tree ()
  (let (( sample-paths
          (list "./path1/somefile.js"
                "./path1/somefile2.js")))
    (should (tree-equal (paths-to-tree sample-paths)
                        '("."
                          ("path1"
                           ;; Reverse ATM
                           ("somefile2.js")
                           ("somefile.js")
                           ))
                        :test 'equal))))

(ert-deftest compress-tree ()
  (let (( tree '("."
                 ("path1"
                  ;; Reverse ATM
                  ("somefile2.js")
                  ("somefile.js")
                  ))))
    (should (equal (compress-tree tree)
                   '("./path1"
                     ;; Reverse ATM
                     ("somefile2.js")
                     ("somefile.js")
                     )))))

(ert-deftest tree-mark-folders ()
  (let (( tree (list "."
                     (list "path1"
                           ;; Reverse ATM
                           (list "somefile2.js")
                           (list "somefile.js")
                           ))))
    (should (equal (tree-mark-folders tree)
                   '("./"
                     ("path1/"
                      ;; Reverse ATM
                      ("somefile2.js")
                      ("somefile.js")
                      ))))))
