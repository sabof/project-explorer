;; -*- lexical-binding: t -*-
(defvar tf/process-and-args
    (list "find" "."))

(defvar tf/get-directory-files-method
  )

(defvar tf/repopulate-function
    (lambda (&rest ignore)
      (let* (( output "")
             ( buffer (current-buffer))
             ( process
               (apply 'start-process "tree-find"
                      buffer tf/process-and-args))
             ( inhibit-read-only t))
        (erase-buffer)
        (insert "Searching for files...")
        (set-process-filter process
                            (lambda (process string)
                              (setq output (concat output string))))
        (set-process-sentinel process
                              (lambda (&rest ignore)
                                (with-current-buffer buffer
                                  (tf/find-output-to-tree output))
                                ;; (message output)
                                ))
        )))

(defvar tf/walker nil
  "Meant to be dynamically bound")

(defvar tf/depth nil
  "Meant to be dynamically bound")

(defun tf/find-output-to-tree (output)
  (setq tmp output)
  (let* (( inhibit-read-only t)
         ( tree (tf/paths-to-tree (split-string output "\n" t)))
         ( compressed-folders (tf/compress-tree tree))
         ( marked-folders (tf/tree-mark-folders compressed-folders))
         ( sorted-folder (tf/sort marked-folders)))
    (erase-buffer)
    (delete-all-overlays)
    ;; (message "%s" output)
    ;; (message "%s" tf/paths-to-tree)
    ;; (prin1 marked-folders (current-buffer))
    ;; (prin1 marked-folders 'insert)
    ;; (princ marked-folders 'insert)
    ;; (insert (format "%s" marked-folders))
    (tf/print-indented-tree sorted-folder)
    (tf/show-first-only) ))

(defun tf/path-to-list (path)
  (let* (( normalized-path
           (replace-regexp-in-string "\\\\" "/" path t t)))
    (split-string normalized-path "/" t)))

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
             (cl-dolist (item level)
               (insert (make-string tf/depth ?\t)
                       (car item)
                       ?\n
                       )
               (when (cdr item)
                 (let ((tf/depth (1+ tf/depth)))
                   (funcall tf/walker (cdr item))))
               ))))
    (funcall tf/walker (rest tree))))

(define-derived-mode tf/mode special-mode
  "Tree find"
  "Display results of find as a folding tree"
  (setq-local revert-buffer-function
              tf/repopulate-function)
  (setq-local tab-width 2)
  (es-define-keys tf/mode-map
    (kbd "<tab>") 'tf/tab)
  (revert-buffer))

(defun tf/closed-p ()
  (let (( ovs (overlays-in (es-total-line-beginning-position)
                           (es-total-line-end-position))))
    (cl-some (lambda (ov)
               (overlay-get ov 'is-tf-hider))
             ovs)))

(defun tf/next-sibling ()
  (interactive)
  (re-search-forward
   (concat "^"
           (make-string
            (es-current-character-indentation)
            ?\t )
           "[^ \t\n]"
           )
   nil t))


(defun tf/open (expanded)
  (interactive "P")
  (save-excursion
    (let (( initial-indentation
            (es-current-character-indentation))
          ( end (save-excursion
                  (or (tf/next-sibling)
                      (goto-char (point-max)))
                  (point))))
      (remove-overlays (es-total-line-beginning-position)
                       (es-total-line-end-position)
                       'is-tf-hider t)
      (unless expanded
        (while (re-search-forward (concat "^"
                                          (make-string (1+ initial-indentation)
                                                       ?\t )
                                          "[^ \t\n]")
                                  end
                                  t)
          (tf/close)))))
  )

  (cl-defun tf/close ()
    (interactive)
    (when (tf/closed-p)
      (cl-return-from tf/close))
    (let* (( indent
             (save-excursion
               (back-to-indentation)
               (buffer-substring (line-beginning-position)
                                 (point))))
           ( end
             (save-excursion
               (goto-char (line-end-position 1))
               (let (( regex
                       (concat "^[\t]\\{0,"
                               (number-to-string
                                (length indent))
                               "\\}[^\n \t]")))
                 (setq tmp regex)
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
  (message "rest: %s" (rest branch))
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
    (message "newrest: %s" new-rest)
    (setcdr branch (mapcar 'tf/sort new-rest))
    branch
    ))

(defun tf/tab (arg)
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

(defun tf/get-filename ()
  (interactive)
  (let (( get-line-filename
          (lambda ()
            (buffer-substring (es-indentation-end-pos)
                              (line-end-position))))
        (tf/walker ))
    (save-excursion
      ())))

(provide 'tree-find)
;;; tree-find.el ends here
