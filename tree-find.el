;; -*- lexical-binding: t -*-
(defvar-local tf/process-and-args
    (list "find" "."))

(defvar-local tf/repopulate-function
    (lambda (&rest ignore)
      (let* (( output "")
             ( buffer (current-buffer))
             ( process
               (apply 'start-process "tree-find"
                      nil tf/process-and-args)))
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
         ( marked-folders (tf/tree-mark-folders compressed-folders)))
    (erase-buffer)
    ;; (message "%s" output)
    ;; (message "%s" tf/paths-to-tree)
    ;; (prin1 marked-folders (current-buffer))
    ;; (prin1 marked-folders 'insert)
    ;; (princ marked-folders 'insert)
    ;; (insert (format "%s" marked-folders))
    (tf/print-indented-tree marked-folders)
    ))

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
  (revert-buffer))

(defun tf/closed-p ()
  (interactive)
  (let (( ovs (overlays-in (line-beginning-position)
                           (line-end-position)
                           )))
    (some (lambda (ov)
            (overlay-get ov 'is-tf-hider))
          ovs)))

(defun tf/open ()
  (interactive)
  )

(defun tf/close ()
  (interactive)
  )

(defun tf/tab ()
  (interactive)
  (if (tf/closed-p)
      (tf/open)
    (tf/close)))

(defun tree-find-sidebar ()
  (interactive)
  (let (( buf (get-buffer-create "*tree-find*")))
    (with-current-buffer buf
      (tf/mode))
    (pop-to-buffer buf)))

(provide 'tree-find)
;;; tree-find.el ends here
