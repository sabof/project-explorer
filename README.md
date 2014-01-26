# project-explorer
A tree project explorer.
![screenshot](https://github.com/sabof/project-explorer/raw/master/screenshot.png)

## Features:
    * Asynchronous indexing
    * Caching
    * Basic file-management
    * I-search support
    * Occur support
    * Folding
    * Grouping of folders containing a single folder
    * Single key navigation keybindings

## Main commands:

    project-explorer-open -- open the sidebar
    project-explorer-helm -- browse the file collection using helm

## Main key-bindings:

    "s"        Change directory
    "j"        Next line
    "k"        Previous line
    "g"        refresh
    "+"        Create file or directory (if the name ends with a slash)
    "-" & "d   Delete file or directory
    "c"        Copy file or directory
    "r"        Rename file or directory
    "q"        hide sidebar
    "u"        go to parent directory
    "["        previous sibling
    "]"        next sibling
    "r"        isearch-backward
    "TAB"      toggle-folding
    "C-U TAB"  unfold descendants
    "RET"      toggle folding of visit file (Specify window with C-U)
    "f"        visit file or directory (Specify window with C-U)
    "w"        show the path of file at point, and copy it to clipboard

## Main customizable variables:

##### pe/project-root-function

    Function that will locate the project root from the current
    `default-directory`

##### pe/directory-tree-function

    Indexing backend. One of `pe/get-directory-tree-async' (native,
    asynchronous), `pe/get-directory-tree-simple'(native, synchronous) or
    `pe/get-directory-tree-external' (extrnal, asynchronus, requires
    "find" with GNU extensions)

##### pe/cache-enabled

    Whether to use caching

##### pe/omit-regex

    Files and directories matching this regex won't be traversed

##### pe/side

    Set to 'left or 'right, depending on which side you want the sidebar to appear

##### pe/width

    The width of the sidebar when it first appears

## Tailor-fitting pe/project-root-function

The default `pe/project-root-function` assumes that the project root will be the closest directory containing `.git`, or the current directory. If you have `projectile` installed, it will simply use `projectile-project-root` instead. Should you need something more specific, it's not hard to make your own project-finding function.

```emacs-lisp
(defun pe/project-root-function-sample ()
  (expand-file-name
   (or
    ;; A specific directory
    (when (string-prefix-p "/path/to/my/project/" default-directory)
      "/path/to/my/project/")
    ;; A directory containg a file
    (locate-dominating-file default-directory "Web.config")
    default-directory)))

(setq pe/project-root-function 'pe/project-root-function-sample)
```
