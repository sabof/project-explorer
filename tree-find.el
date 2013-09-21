;; -*- lexical-binding: t -*-
(defvar tf/process-and-args
  (list "find" "."))

(defvar tf/get-directory-files-method
  "find . \\( ! -path '*/.*' \\)"
  "The \"backend\" for tree-find.
  Can be a string, or a function.

  The string will be executed in bash, and it's output should be a
  list of files separated by newlines.

  The fucntion takes 2 arguments, and should return a list of
  files. It can also return nil - in which case you are on your
  own, as there will be no furhter processing. The arguments are
  the directory to scan, and depth. 0 means don't limit depth")


(defvar tf/repopulate-function
  (lambda (&rest ignore)
    ))

(cl-defun tf/revert-buffer (&rest ignore)
  (let (( inhibit-read-only t)
        ;; ( shell
        ;;   (or explicit-shell-file-name
        ;;       (getenv "ESHELL")
        ;;       shell-file-name))
        )
    (erase-buffer)
    (delete-all-overlays)
    (insert "Searching for files...")
    (when (functionp tf/get-directory-files-method)
      (let (( function-result
              (funcall tf/get-directory-files-method
                       default-directory
                       0)))
        (when function-result
          (tf/find-output-to-tree function-result)))
      (cl-return-from tf/revert-buffer))
    (let* (( output "")
           ( buffer (current-buffer))
           ( process
             (start-process "tree-find"
                            buffer "bash" "-c" tf/get-directory-files-method))
           ( inhibit-read-only t))
      (set-process-filter process
                          (lambda (process string)
                            (setq output (concat output string))))
      (set-process-sentinel process
                            (lambda (&rest ignore)
                              (let ((inhibit-read-only t))
                                (with-current-buffer buffer
                                  (if (string-match-p "[^\n\t ]"
                                                      output)
                                      (tf/find-output-to-tree output)
                                    (erase-buffer)
                                    (insert "No files were found"))))
                              ))
      )))

(defvar tf/walker nil
  "Meant to be dynamically bound")

(defvar tf/depth nil
  "Meant to be dynamically bound")

(defun tf/find-output-to-tree (output)
  ;; (setq tmp output)
  (let* (( inhibit-read-only t)
         ( tree (tf/paths-to-tree
                 (if (stringp output)
                     (split-string output "\n" t)
                   output)))
         ( compressed-folders (tf/compress-tree tree))
         ( marked-folders (tf/tree-mark-folders compressed-folders))
         ( sorted-folder (tf/sort marked-folders)))
    (erase-buffer)
    (tf/print-indented-tree sorted-folder)
    (tf/show-first-only) ))

(defun tf/path-to-list (path)
  (let* (( normalized-path
           (replace-regexp-in-string "\\\\" "/" path t t)))
    (split-string normalized-path "/" t)))

(defun tf/tab-ending ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward "\t")
    (point)))

(defun tf/current-indnetation ()
  (- (tf/tab-ending)
     (line-beginning-position)))

(defun tf/paths-to-tree (paths)
  (let* (( paths (mapcar 'tf/path-to-list paths))
         ( add-member (lambda (what where)
                        (setcdr where (cons what (cdr where)))
                        what))
         ( root (list nil))
         head)
    (cl-dolist (path paths)
      (setq head root)
      (cl-dolist (segment path)
        (setq head (or (cl-find segment
                                (rest head)
                                :test 'equal
                                :key 'car)
                       (funcall add-member
                                (list segment)
                                head)))))
    (cadr root)
    ;; (cdr root)
    ))

(defun tf/tree-mark-folders (tree)
  (let (( tf/walker
          (lambda (branch)
            (when (rest branch)
              (setcar branch (concat (car branch) "/")))
            (mapcar tf/walker (rest branch)))))
    (funcall tf/walker tree)
    tree))

(defun tf/compress-tree (path-tree)
  (let* (( tf/walker
           (lambda (branch)
             (cond ( (= (length branch) 1)
                     branch)
                   ( (and (= (length branch) 2)
                          (cl-cadadr branch))
                     (funcall tf/walker (cons (concat (car branch)
                                                      "/"
                                                      (cl-caadr branch))
                                              (cl-cdadr branch))))
                   ( t (cons (car branch)
                             (mapcar tf/walker (cdr branch))))))))
    (funcall tf/walker path-tree)))

(defun tf/print-indented-tree (tree)
  (let* (( tf/depth 0)
         ( tf/walker
           (lambda (level)
             (cl-loop for item in level
                      for counter = 0 then (1+ counter)
                      do
                      (insert (make-string tf/depth ?\t))
                      (if (tf/directory-branch-p item)
                          (progn
                            (insert-text-button
                             "►"
                             'action
                             (lambda (marker)
                               (goto-char (marker-position marker))
                               (tf/tab))
                             )
                            (insert " ")
                            (insert-text-button (car item)
                                                'action
                                                (lambda (marker)
                                                  (goto-char (marker-position marker))
                                                  (setq default-directory
                                                        (tf/get-filename))
                                                  (revert-buffer))))
                        (insert-text-button (car item)
                                            'action
                                            (lambda (marker)
                                              (goto-char (marker-position marker))
                                              (find-file-other-window
                                               (tf/get-filename)))))
                      (insert ?\n)


                      (when (cdr item)
                        (let ((tf/depth (1+ tf/depth)))
                          (funcall tf/walker (cdr item))))
                      ))))
    (funcall tf/walker (rest tree))))

(define-derived-mode tf/mode special-mode
  "Tree find"
  "Display results of find as a folding tree"
  (setq-local revert-buffer-function
              'tf/revert-buffer)
  (setq-local tab-width 2)
  (es-define-keys tf/mode-map
    (kbd "u") 'tf/up-element
    (kbd "<tab>") 'tf/tab
    (kbd "M-}") 'tf/forward-element
    (kbd "M-{") 'tf/backward-element
    )
  (revert-buffer))

(defun tf/closed-p ()
  (let (( ovs (overlays-in (es-total-line-beginning-position)
                           (es-total-line-end-position))))
    (cl-some (lambda (ov)
               (overlay-get ov 'is-tf-hider))
             ovs)))

(defun tf/forward-element (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (let* (( initial-indentation
           (es-current-character-indentation))
         ( regex
           (concat "^\t\\{0,"
                   (int-to-string
                    initial-indentation)
                   "\\}[^\t\n]"
                   )))
    (if (cl-minusp arg)
        (goto-char (line-beginning-position))
      (goto-char (line-end-position)))
    (when (re-search-forward regex nil t arg)`
      (goto-char (match-end 0))
      (forward-char -1)
      )))

(defun tf/backward-element (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (tf/forward-element (- arg)))

(defun tf/open (expanded)
  (interactive "P")
  (save-excursion
    (let (( initial-indentation
            (es-current-character-indentation))
          ( end (save-excursion
                  (or (tf/forward-element)
                      (goto-char (point-max)))
                  (point))))
      (remove-overlays (es-total-line-beginning-position)
                       (es-total-line-end-position)
                       'is-tf-hider t)
      (unless expanded
        (while (re-search-forward (concat "^"
                                          (make-string (1+ initial-indentation)
                                                       ?\t )
                                          "[^\t\n]")
                                  end
                                  t)
          (tf/close)))))
  )

(cl-defun tf/close ()
  (interactive)
  (when (or (looking-at ".*\n?\\'")
            (tf/closed-p))
    (cl-return-from tf/close))
  (let* (( indent
           (save-excursion
             (goto-char (line-beginning-position))
             (skip-chars-forward "\t")
             ;; (back-to-indentation)
             (buffer-substring (line-beginning-position)
                               (point))))
         ( end
           (save-excursion
             (goto-char (line-end-position 1))
             (let (( regex
                     (concat "^[\t]\\{0,"
                             (number-to-string
                              (length indent))
                             "\\}[^\n\t]")))
               ;; (setq tmp regex)
               (if (re-search-forward regex nil t)
                   (line-end-position 0)
                 (point-max)))))
         ( ov (make-overlay (line-end-position 1)
                            end)))
    (overlay-put ov 'isearch-open-invisible-temporary
                 'hs-isearch-show-temporary)
    (overlay-put ov 'isearch-open-invisible
                 'hs-isearch-show)
    (overlay-put ov 'invisible
                 'hs)
    (overlay-put ov 'display "...")
    (overlay-put ov 'hs
                 'code)
    (overlay-put ov 'is-tf-hider t)
    (overlay-put ov 'evaporate t)
    ))

(defun tf/show-first-only ()
  (interactive)
  (save-excursion
    (remove-overlays (point-min) (point-max)
                     'is-tf-hider t)
    (goto-char (point-min))
    (while (re-search-forward "^[^\t]" nil t)
      (tf/close))))

(defun tf/directory-branch-p (branch)
  (rest branch))

(defun tf/sort (branch)
  (let (( new-rest
          (sort (rest branch)
                (lambda (a b)
                  (cond ( (and (tf/directory-branch-p a)
                               (not (tf/directory-branch-p b)))
                          t)
                        ( (and (not (tf/directory-branch-p a))
                               (tf/directory-branch-p b))
                          nil)
                        ( t (string< (car a) (car b))))))))
    (setcdr branch (mapcar 'tf/sort new-rest))
    branch
    ))

(defun tf/tab (&optional arg)
  (interactive "P")
  (if (tf/closed-p)
      (tf/open arg)
    (tf/close)))

(defun tree-find-sidebar ()
  (interactive)
  (let (( buf (get-buffer-create "*tree-find*"))
        ( parent-directory default-directory))
    (with-current-buffer buf
      (setq default-directory parent-directory)
      (tf/mode))
    (pop-to-buffer buf)))

(defun tf/up-element ()
  (interactive)
  (goto-char (es-total-line-beginning-position))
  (let (( indentation (tf/current-indnetation)))
    (when  (and (not (zerop indentation))
                (re-search-backward
                 (format
                  "^\\(?1:\t\\{0,%s\\}\\)[^\t\n]"
                  (number-to-string (1- indentation)))
                 nil t))
      (goto-char (match-end 1)))))

(defun tf/get-filename ()
  (interactive)
  (let* (( get-line-text
           (lambda ()
             (goto-char (es-total-line-beginning))
             (skip-chars-forward "\t ►")
             (buffer-substring-no-properties
              (point) (line-end-position))))
         ( result
           (funcall get-line-text)))
    (save-excursion
      (while (tf/up-element)
        (setq result (concat (funcall get-line-text)
                             result))))
    (expand-file-name result)))

(provide 'tree-find)
;;; tree-find.el ends here
