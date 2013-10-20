(ert-deftest pe/compress-tree ()
  (let (( tree1 '("path0"
                  ("path1"
                   ;; Reverse ATM
                   "somefile2.js"
                   "somefile.js"
                   )))
        ( tree2 '("tree-find"
                  (".git"
                   ("branches")
                   ("hooks" "applypatch-msg.sample" "commit-msg.sample" "post-update.sample"
                    "pre-applypatch.sample" "pre-commit.sample" "pre-rebase.sample"
                    "prepare-commit-msg.sample" "update.sample")
                   ("info" "exclude")
                   ("logs"
                    ("refs"
                     ("heads" "master")
                     ("remotes"
                      ("origin" "master")))
                    "HEAD")
                   ("objects"
                    ("02" "eb577bf06c32ebdbb61eb7bb6b0f2b7b4806ec")
                    ("06" "19b50489a87218d93cc220f26bca1472105624"
                          "9541c465d7fe5547f11244a813b8fa7db5a90d"
                          "b012dd1655736a925d7becee8c93edece62187"
                          "fe8f926f7fdc8b85da2fa4e32452759152b79b")
                    ("0d" "0b61f84a5fa9a0dcbad952719cf07568c9c12b")
                    ("0f" "4ef8557729ab41fa4ce52af459e5475a99f984")
                    ("12" "47201f20518bb3ce6089c170f117fd0a9bdd91")
                    ("16" "86c4f4b2ea5d7cbfbd186ffb4e899749a95efe")
                    ("19" "19f6b6fd369d4bd51d0fd55bb5b973cf522a5d")
                    ("22" "fa47a4f71f49c5aa48438c2bbf84df0ae2fc8d")
                    ("24" "083aafc32eb5682a9c6f26c63ee5e6fd39a9f4")
                    ("25" "a1672e4bbb9a0a448a6effd097bf975afe3cb9")
                    ("2b" "c8a1bfa465f88c2aed51dc8c54a27b8734624b")
                    ("31" "7343f533759733faa7e1adb2549d8e0ca54954")
                    ("3d" "17331266e997fa249a8231e3aa70f48f2a8e53")
                    ("3e" "18742baa20d7b2d4e8aad0c4d9c27c3dd2b1b1")
                    ("3f" "e8300614f9b07307938248c2ae47643b745141")
                    ("41" "c15a76f36971b2249e3c0a5dece851556da378")
                    ("45" "e94200e62285481c776068ea0488f2d01be2fe")
                    ("47" "0f3c966adfd69de7175ff1e981b695fbe9d945")
                    ("48" "97edc8558c2f8bc2b95b8d60379cd5a795bfd3")
                    ("4b" "825dc642cb6eb9a060e54bf8d69288fbee4904")
                    ("4c" "70cc142b55843d971aefbd98c6fe89c84b52ca")
                    ("55" "3625961326ebde210905fa6f0bfdc7a02a9ffb")
                    ("58" "e68c048106dc1bed1ab9c7526fc9e17ceeec7f")
                    ("5e" "a1cbdc8a2ef423db491a4a870f94103f54d515")
                    ("68" "53a8bc83b368b6c88c448b785ca2403a7c379b")
                    ("69" "5ee2359b34cd1633f676fa702f9fe609a1b5b3")
                    ("6d" "d8baac04cfdbdeac1ca382d7d5e65932a59515")
                    ("6e" "adc941698a2fc724471a4e3309a6e8310c527f")
                    ("7a" "3d5563d83574bf2196a1b464fbfc371ccd403f")
                    ("87" "0d508e7f16e8684204354e68beeb49c06630e5")
                    ("88" "3fcbf1d45294b6976686658d6c45f6119c072d")
                    ("8b" "9fc1c2e75b7b853478cfa23d6aca0f2263adb6")
                    ("8c" "7f7c51269e5c69c2ca7a73ec521e5ceed4ea8d")
                    ("91" "a92b0fbd1537801eeff11682c3d6821dc06ab3")
                    ("96" "b9aac93649b9dfb060aaf874768315c0e6b9d5")
                    ("98" "76725c9e48fbdde1788fb7795bad40b5fa7a68")
                    ("99" "7f59669fe47fc6b573efd72e6efc36798d96d1")
                    ("9a" "078030a6bc88f4d7e68131bfae80fb232e3c05")
                    ("9b" "25dcad4450755355b557c733c868d42d52fac7")
                    ("a1" "287ea9a6f1b06a2180777b0c1d58ff247cd374")
                    ("a2" "bd533f4b50f7c2f8bf4e95f0b96fbdbba24619")
                    ("a6" "3ee090c1e33ef87608b4d974aedddbf0264a13")
                    ("a7" "6e1c014ff62edc1cdd3c8d030f42dcf50def7d" "7137841d9e9c22a09cad72f10cf3d8e7edcd72")
                    ("ac" "cd4679767a2011803fe5b5e11ea8396a158334")
                    ("b4" "89168fa243003da6cfb5854fd996684e5a160e")
                    ("b5" "90a5ede42bc18fa7199ca16d21a02a0803bc0e")
                    ("be" "1636e3301f897f1e416587f7df69dc64775ffe")
                    ("bf" "b465570551766fb10213ab7828bcadb3f85140")
                    ("c1" "6838eabf39e830814479f0f860998fe9c78619")
                    ("c5" "38718fcb347d62902876d57191302d40aa8854")
                    ("c6" "750db48db77f9a72a8a96673d22e1dbeb17b1f" "bced70d7ac4825e0d5d1c6b4138da836d9fedb")
                    ("cb" "fad83a78dc9534817ffb1841fe35c0b6e43203")
                    ("cc" "0573d0039c710179fe8c1ddfee92a71bc8c9e1")
                    ("cf" "3c6d15e27e97e2b98f34d14a1f22b3277f6563" "819160986dafe53372ddb3a891c8634235cfd4")
                    ("d7" "b19eecfb2a5dfa9851ae231e698db717674d16")
                    ("d8" "c1519a4d1057b553d4cd0b5f7b879ef9fcab5f")
                    ("df" "84da5be54ef026666bceaee64a78334fa1539b")
                    ("e9" "68625e71e6d5862a2ff2275351b6de5b9cf09d")
                    ("eb" "9a85f5fa1aa0ce4a4da85a7503c57ca73947b9")
                    ("ee" "7694397e386ad1e0056d97d16b2cd19e143987")
                    ("ef" "600b47f58b5f9b0e5893878ce372cc4211303e" "68c650a012c05541a2ab46d2ae9c90858b669b")
                    ("f1" "8de63871a67f3bba68d7aa06ac1c1ea3855521")
                    ("f2" "d5e4f7506910e95bd4aecde36dc168c9acae05")
                    ("f3" "944bcbb5cf6c2614f61841f358f637ed2cba11")
                    ("f6" "c4d71a746b257e72e0b86be5bc8f5e0f2c4607")
                    ("fb" "6b6361366e07cd579812844095bf95ddb07353")
                    ("info")
                    ("pack"))
                   ("refs"
                    ("heads" "master")
                    ("remotes"
                     ("origin" "master"))
                    ("tags"))
                   "COMMIT_EDITMSG" "FETCH_HEAD" "HEAD" "ORIG_HEAD" "config" "description" "index")
                  "README.md" "tests.el" "tree-find.el")))
    (should (tree-equal (pe/compress-tree tree1)
                        '("path0/path1"
                          ;; Reverse ATM
                          "somefile2.js"
                          "somefile.js")
                        :test 'string-equal))
    (should (consp (ignore-errors (pe/compress-tree tree2))))))

(ert-deftest pe/sort ()
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
         ( result (pe/sort (copy-tree alist))))
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

(ert-deftest pe/print-indented-tree ()
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
             (pe/print-indented-tree tree)
             (buffer-string))))
    (should (string-equal expected-result
                          result))))

(ert-deftest pe/forward-element ()
  (with-temp-buffer
    (insert "	index1
	index2
dir-cache.el
tests.el
")

    (goto-char (point-min))
    (pe/forward-element)
    (should (= 2 (line-number-at-pos)))
    (let ((ori-point (point)))
      (back-to-indentation)
      (should (= (point) ori-point)))

    (goto-char (1- (point-max)))
    (pe/forward-element -1)
    (should (= 3 (line-number-at-pos)))
    (let ((ori-point (point)))
      (back-to-indentation)
      (should (= (point) ori-point)))))

(ert-deftest pe/flatten-tree1 ()
  (let* ((input (list "def"
                      "ab"
                      "cd"))
         (output (pe/flatten-tree input))
         (expected-output (list "def/ab"
                                "def/cd")))
    (should (tree-equal output expected-output
                        :test 'string-equal))))

(ert-deftest pe/flatten-tree2 ()
  (let* ((input (list "def"
                      "ab"
                      "cd"))
         (output (pe/flatten-tree input "abc"))
         (expected-output (list "abc/def/ab"
                                "abc/def/cd")))
    (should (tree-equal output expected-output
                        :test 'string-equal))))

(ert-deftest pe/flatten-tree3 ()
  (let* ((input (list "def"
                      "ab"
                      "cd"))
         (output (pe/flatten-tree input "abc"))
         (expected-output (list "abc/def/ab"
                                "abc/def/cd")))
    (should (tree-equal output expected-output
                        :test 'string-equal))))

(ert-deftest pe/goto-file ()
  (let ((text "node2/
node1/
node3/
	subnode1/
		subnode3/
		subnode2/
")
        )
    (with-temp-buffer
      (insert text)
      (pe/goto-file "node1")
      (should (= (point) 8))
      (pe/goto-file "node3/subnode1")
      (should (= (point) 23))
      (pe/goto-file "node3/subnode1/subnode3")
      (should (= (point) 35))
      (pe/goto-file "node3/subnode1/subnode3/")
      (should (= (point) 35))
      (pe/goto-file "node3/subnode1/subnode2/")
      (should (= (point) 47))
      )))

(ert-deftest pe/goto-file2 ()
  (let ((text "node2/
node1/
node3/
	node1/
		node3/
		node2/
node4/
	node6/
		node3/
		node2/
node5/node6/
	node7/
		node8/
node6/node6/
	node7/
		node8/
data/43/d0ce42-4d9a-4fb8-b6da-073363c8c4f2/
	tumblr_mdefifNYFD1qzfvn2o2_250.jpg
")
        )
    (with-temp-buffer
      (insert text)
      (goto-char 1)

      (should-not (pe/goto-file "node3/node6/"))
      (should (= (point) 1))

      (should (pe/goto-file "node5/node6/node7/"))
      (should (= (point) 95))

      (should (pe/goto-file "node5/node6/node7/node8/"))
      (should (= (point) 104))

      (should (pe/goto-file "node5/"))
      (should (= (point) 81))

      (should (pe/goto-file "node5/node6/"))
      (should (= (point) 87))

      (should (equal (pe/goto-file
                      (concat "data/43/d0ce42-4d9a-4fb8-b6da-073363c8c4f2/"
                              "tumblr_mdefifNYFD1qzfvn2o2_250.jpg"))
                     186))
      )))

(defun pe/integration-test ()
  (let (( pe/get-directory-files-method
          (lambda (dir func)
            (funcall func '("root"
                            ("node2")
                            ("node1")
                            "node4"
                            ("node3"
                             ("subnode1"
                              ("subnode3")
                              ("subnode2"))))))))
    (tree-find-open)
    ))
