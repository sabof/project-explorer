;;; tree-find.el --- Find-based project explorer -*- lexical-binding: t -*-
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/tree-find
;;; Package-Requires: ((cl-lib "1.0") (es-lib "0.3"))

;;; Commentary:

;; The project is hosted at https://github.com/sabof/tree-find
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib)

(defvar tf/directory-files-function
  'tf/get-directory-tree-simple)
(defvar tf/side 'left)
(defvar tf/width 40)
(defvar tf/omit t)
(defvar tf/omit-regex
  "^\\.\\|^#")

(defvar-local tf/data nil)
(defvar-local tf/unfolded-lines nil)
(defvar-local tf/previous-directory nil)

(defvar tf/project-root-function
  (lambda ()
    (if (fboundp 'projectile-project-root)
        (projectile-project-root)
      (locate-dominating-file default-directory ".git"))))

(cl-defun tf/get-directory-tree-simple (dir done-func)
  ;; (unless (file-exists-p dir)
  ;;   (debug))
  (let (walker)
    (setq walker
          (lambda (dir)
            (let (( files (dc//directory-files dir)))
              (cons (file-name-nondirectory (directory-file-name dir))
                    (mapcar (lambda (file)
                              (if (file-directory-p (concat dir file))
                                  (funcall walker (concat dir file "/"))
                                file))
                            files)))))
    (funcall done-func (funcall walker dir))))

(defun tf/get-tree-find-buffers ()
  (es-buffers-with-mode 'tf/mode))

(cl-defun tf/revert-buffer (&rest ignore)
  (let (( inhibit-read-only t)
        ( tree-find-buffer (current-buffer))
        ( starting-name (tf/get-filename))
        ( starting-column (current-column)))
    (erase-buffer)
    (delete-all-overlays)
    (insert "Searching for files...")
    (funcall tf/directory-files-function
             default-directory
             (lambda (result)
               (when result
                 (with-current-buffer tree-find-buffer
                   (setq tf/data result)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (tf/print-indented-tree
                      (tf/compress-tree
                       (tf/sort result)))
                     (font-lock-fontify-buffer)
                     (goto-char (point-min)))
                   ))))
    (if (not (string-equal tf/previous-directory
                           default-directory))
        (setq tf/unfolded-lines)
      (progn
        (setq tf/unfolded-lines
              (cl-remove-if-not (lambda (file-name)
                                  (save-excursion
                                    (and (tf/goto-file file-name)
                                         (tf/foldable-p))))
                                tf/unfolded-lines))
        (mapc (lambda (file-name)
                (when (tf/goto-file file-name)
                  (tf/unfold)))
              (cl-sort
               tf/unfolded-lines
               '<
               :key (lambda (it) (length (split-string it "/" t)))
               )))
      (when starting-name
        (tf/goto-file starting-name nil t)
        (move-to-column starting-column)))
    (setq tf/previous-directory default-directory)
    (when (eq this-command 'revert-buffer)
      (message "Refresh complete"))
    ))

(defun tf/file-interesting-p (name)
  (if tf/omit
      (not (string-match-p tf/omit-regex name))
    t))

(cl-defun tf/compress-tree (branch)
  (cond ( (not (consp branch))
          branch)
        ( (= (length branch) 1)
          branch)
        ( (and (= (length branch) 2)
               (consp (cl-second branch)))
          (tf/compress-tree
           (cons (concat (car branch) "/" (cl-caadr branch))
                 (cl-cdadr branch))))
        ( t (cons (car branch)
                  (mapcar 'tf/compress-tree (cdr branch))))))

(cl-defun tf/sort (branch)
  (when (stringp branch)
    (cl-return-from tf/sort branch))
  (let (( new-rest
          (sort (rest branch)
                (lambda (a b)
                  (cond ( (and (consp a)
                               (stringp b))
                          t)
                        ( (and (stringp a)
                               (consp b))
                          nil)
                        ( (and (consp a) (consp b))
                          (string< (car a) (car b)))
                        ( t (string< a b)))))))
    (setcdr branch (mapcar 'tf/sort new-rest))
    branch
    ))

(cl-defun tf/print-indented-tree (branch &optional (depth -1))
  (let (start)
    (cond ( (stringp branch)
            (when (tf/file-interesting-p branch)
              (insert (make-string depth ?\t)
                      branch
                      ?\n)))
          ( t (when (or (> 0 depth)
                        (tf/file-interesting-p (car branch)))
                (when (>= depth 0)
                  (insert (make-string depth ?\t)
                          (car branch) "/\n")
                  (setq start (point)))
                (cl-dolist (item (rest branch))
                  (tf/print-indented-tree item (1+ depth)))
                (when (and start (> (point) start))
                  ;; (message "ran %s %s" start (point))
                  (tf/make-hiding-overlay (1- start) (1- (point)))
                  )
                )))))

;;; TEXT ->

(defun tf/current-indnetation ()
  (- (tf/tab-ending)
     (line-beginning-position)))

(defun tf/tab-ending ()
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward "\t")
    (point)))

(cl-defun tf/unfold (&optional expanded)
  (interactive "P")
  (let (( line-beginning
          (es-total-line-beginning-position)))
    (when (/= (line-number-at-pos)
              (line-number-at-pos
               line-beginning))
      (goto-char line-beginning)
      (goto-char (1- (line-end-position)))
      ))
  (cl-pushnew (tf/get-filename) tf/unfolded-lines
              :test 'string-equal)
  (save-excursion
    (let* (( initial-indentation
             (es-current-character-indentation))
           ( end (save-excursion
                   (or (tf/forward-element)
                       (point-max))))
           ( ov (save-excursion
                  (goto-char (es-total-line-beginning-position))
                  (car (overlays-at (line-end-position))))))
      (when ov
        (delete-overlay ov))
      ;; (remove-overlays (es-total-line-beginning-position)
      ;;                  (es-total-line-end-position)
      ;;                  'is-tf-hider t)
      ;; (unless expanded
      ;;   (while (re-search-forward
      ;;           (format "^\t\\{%s\\}[^\t\n].*/$" (1+ initial-indentation))
      ;;           end
      ;;           t)
      ;;     (tf/fold)))
      )))

(defun tf/make-hiding-overlay (from to)
  (let ((ov (make-overlay from to)))
    (overlay-put ov 'isearch-open-invisible-temporary
                 'hs-isearch-show-temporary)
    (overlay-put ov 'isearch-open-invisible
                 'hs-isearch-show)
    (overlay-put ov 'invisible 'hs)
    (overlay-put ov 'display "...")
    (overlay-put ov 'hs 'code)
    (overlay-put ov 'is-tf-hider t)
    (overlay-put ov 'evaporate t)
    )
  )

(defun tf/goto-file (file-name &optional on-each-semgent-function goto-best-match)
  ;; FIXME: Doesn't work with combined segments
  (let* (( segments (split-string
                     (if (file-name-absolute-p file-name)
                         (substring file-name (length default-directory))
                       file-name)
                     "/" t))
         ( init-pos (point))
         best-match
         found)
    (goto-char (point-min))
    (cl-loop with limit
             for segment in segments
             for iter = 0 then (1+ iter)
             ;; for is-final = (= iter (1- (length segments)))
             do (cond ( (and (cl-plusp iter)
                             (save-excursion
                               (goto-char (match-end 1))
                               (save-match-data
                                 (looking-at (concat (regexp-quote segment) "/$")))))
                        (goto-char (match-end 1))
                        (setq best-match (match-end 1))
                        (cl-decf iter))
                      ( (re-search-forward
                         (format "^\t\\{%s\\}\\(?1:%s/?\\)"
                                 (int-to-string iter) segment)
                         limit t)
                        ;; (goto-char (match-beginning 1))
                        (setq limit (save-excursion
                                      (or (tf/forward-element)
                                          (point-max))))
                        (setq best-match (match-beginning 1))
                        (when on-each-semgent-function
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (funcall on-each-semgent-function))))
                      ( t (cl-return)))
             finally (setq found t))
    (goto-char (if (and best-match (or found goto-best-match))
                   best-match
                 init-pos))
    (and found best-match)))

(defun tf/show-file (file-name)
  (tf/goto-file file-name 'tf/unfold))

(cl-defun tf/foldable-p ()
  (unless (looking-at ".*/$")
    (cl-return-from tf/foldable-p nil))
  (let ((init-line (line-number-at-pos))
        (next-line (save-excursion
                     (tf/forward-element)
                     (line-number-at-pos))))
    (and (/= init-line next-line)
         (/= (1+ init-line) next-line))
    ))

(cl-defun tf/fold ()
  (interactive)
  (when (or (looking-at ".*\n?\\'")
            (tf/folded-p))
    (cl-return-from tf/fold))
  (let* (( indent
           (save-excursion
             (goto-char (line-beginning-position))
             (skip-chars-forward "\t")
             (buffer-substring (line-beginning-position)
                               (point))))
         ( end
           (save-excursion
             (goto-char (line-end-position 1))
             (let (( regex
                     (format "^\t\\{0,%s\\}[^\t\n]"
                             (length indent))))
               (if (re-search-forward regex nil t)
                   (line-end-position 0)
                 (point-max)))))
         ( name (tf/get-filename))
         ( unfolded-contained
           (progn
             (setq tf/unfolded-lines
                   (cl-remove name tf/unfolded-lines
                              :test 'string-equal))
             (cl-sort (cl-remove-if-not
                       (lambda (path)
                         (string-prefix-p name path))
                       tf/unfolded-lines)
                      '>
                      :key (lambda (it) (length (split-string it "/" t)))
                      ))))
    (save-excursion
      (cl-dolist (file-name unfolded-contained)
        (when (tf/goto-file file-name)
          (tf/fold))))
    (tf/make-hiding-overlay (line-end-position 1)
                            end)
    ))

(defun tf/up-directory ()
  (interactive)
  (setq default-directory
        (file-name-directory
         (directory-file-name
          default-directory)))
  (revert-buffer))

(defun tf/folded-p ()
  (let (( ovs (save-excursion
                (goto-char (es-total-line-beginning-position))
                (goto-char (line-end-position))
                (overlays-at (point)))))
    (cl-some (lambda (ov)
               (overlay-get ov 'is-tf-hider))
             ovs)))

(defun tf/quit ()
  (interactive)
  (let ((window (selected-window)))
    (quit-window)
    (when (window-live-p window)
      (delete-window))))

(defun tf/forward-element (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (save-match-data
    (let* (( initial-indentation
             (es-current-character-indentation))
           ( regex (format "^\t\\{0,%s\\}[^\t\n]"
                           initial-indentation)))
      (if (cl-minusp arg)
          (goto-char (line-beginning-position))
        (goto-char (line-end-position)))
      (when (re-search-forward regex nil t arg)
        (goto-char (match-end 0))
        (forward-char -1)
        (point)))))

(defun tf/backward-element (&optional arg)
  (interactive "p")
  (setq arg (or arg 1))
  (tf/forward-element (- arg)))

(defun tf/middle-click (event)
  (interactive "e")
  (mouse-set-point event)
  (tf/return))

(defun tf/return ()
  (interactive)
  (if (file-directory-p (tf/get-filename))
      (tf/tab)
    (tf/find-file)))

(defun tf/find-file (&optional focus)
  (interactive)
  (let ((file-name (tf/get-filename))
        (win (cadr (window-list))))
    (select-window win)
    (find-file file-name)))

(defun tf/tab (&optional arg)
  (interactive "P")
  (if (tf/folded-p)
      (tf/unfold arg)
    (tf/fold)))

(defun tf/up-element ()
  (interactive)
  (goto-char (es-total-line-beginning-position))
  (let (( indentation (tf/current-indnetation)))
    (and (not (zerop indentation))
         (re-search-backward (format
                              "^\\(?1:\t\\{0,%s\\}\\)[^\t\n]"
                              (1- indentation))
                             nil t)
         (goto-char (match-end 1)))))

(defun tf/get-filename ()
  (interactive)
  (save-excursion
    (let* (( get-line-text
             (lambda ()
               (goto-char (es-total-line-beginning))
               (skip-chars-forward "\t ")
               (buffer-substring-no-properties
                (point) (line-end-position))))
           ( result
             (funcall get-line-text)))
      (while (tf/up-element)
        (setq result (concat (funcall get-line-text)
                             result)))
      (setq result (expand-file-name result))
      (when (file-directory-p result)
        (setq result (file-name-as-directory result)))
      result)))

(defun tf/set-directory (dir)
  (interactive
   (let ((file-name (tf/get-filename)))
     (list (read-file-name
            "Set directory to: "
            (if (file-directory-p file-name)
                file-name
              (file-name-directory
               (directory-file-name
                file-name)))))))
  (unless (file-directory-p dir)
    (error "\"%s\" is not a directory"
           dir))
  (setq dir (file-name-as-directory dir))
  (unless (string-equal default-directory dir)
    (setq tf/unfolded-lines))
  (setq default-directory dir)
  (revert-buffer)
  )

(define-derived-mode tf/mode special-mode
  "Tree find"
  "Display results of find as a folding tree"
  (setq-local revert-buffer-function
              'tf/revert-buffer)
  (setq-local tab-width 2)
  (es-define-keys tf/mode-map
    (kbd "u") 'tf/up-element
    (kbd "d") 'tf/set-directory
    (kbd "<tab>") 'tf/tab
    (kbd "M-}") 'tf/forward-element
    (kbd "M-{") 'tf/backward-element
    (kbd "]") 'tf/forward-element
    (kbd "[") 'tf/backward-element
    (kbd "n") 'next-line
    (kbd "p") 'previous-line
    ;; (kbd "^") 'tf/up-directory
    (kbd "<return>") 'tf/return
    (kbd "<mouse-2>") 'tf/middle-click
    (kbd "q") 'tf/quit
    (kbd "s") 'isearch-forward
    (kbd "r") 'isearch-backward
    (kbd "f") 'tf/find-file
    )
  (font-lock-add-keywords
   'tf/mode '(("^.+/$" (0 'dired-directory append)))))

(defun tf/get-current-tree-find-buffer ()
  (let (( project-root (funcall tf/project-root-function))
        ( tree-find-buffers (tf/get-tree-find-buffers)))
    (cl-find project-root
             tree-find-buffers
             :key (lambda (project-tree-find-buffer)
                    (with-current-buffer project-tree-find-buffer
                      default-directory))
             :test 'string-equal)))

(defun tf/flatten-tree (tree &optional prefix)
  (let ((current-prefix (if prefix
                            (concat prefix "/" (car tree))
                          (car tree))))
    (cl-reduce 'append
               (mapcar (lambda (it)
                         (if (consp it)
                             (tf/flatten-tree it current-prefix)
                           (list (concat current-prefix "/" it))))
                       (rest tree))))
  )

(defun tf/completing-read-files ()
  (let ((flat-tree (with-current-buffer tf/get-current-tree-find-buffer
                     ))
        (completing-read ))
    ))

;;; Interface

(cl-defun tree-find-open ()
  (interactive)
  (let* (( project-root (funcall tf/project-root-function))
         ( tree-find-buffers (tf/get-tree-find-buffers))
         ( project-tree-find-existing-buffer
           (cl-find project-root
                    tree-find-buffers
                    :key (lambda (project-tree-find-buffer)
                           (with-current-buffer project-tree-find-buffer
                             default-directory))
                    :test 'string-equal))
         ( tree-find-window
           (and project-tree-find-existing-buffer
                (get-window-with-predicate
                 (lambda (tree-find-window)
                   (eq (window-buffer tree-find-window)
                       project-tree-find-existing-buffer))
                 (window-list))))
         ( project-tree-find-buffer
           (or project-tree-find-existing-buffer
               (with-current-buffer
                   (generate-new-buffer "*tree-find*")
                 (tf/mode)
                 (setq default-directory project-root)
                 (revert-buffer)
                 (current-buffer)))))

    (when tree-find-window
      (select-window tree-find-window)
      (cl-return-from tree-find-open))

    (cl-dolist (window (window-list))
      (and (memq (window-buffer window) tree-find-buffers)
           (> (length (window-list)) 1)
           (delete-window window)))
    (setq tree-find-window (split-window (frame-root-window)
                                         (- (frame-width) tf/width) tf/side))
    (set-window-parameter tree-find-window 'window-side tf/side)
    (set-window-buffer tree-find-window project-tree-find-buffer)
    (set-window-dedicated-p tree-find-window t)
    (select-window tree-find-window)
    ))

(provide 'tree-find)
;;; tree-find.el ends here
