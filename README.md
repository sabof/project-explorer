# project-explorer
A tree project explorer.
![screenshot](https://github.com/sabof/project-explorer/raw/master/screenshot.png)

##Features:

* Folding
* Grouping of folders containing a single folder
* I-search support
* Occur support
* Single key navigation keybindings
* Caching
* Multiple indexing methods, some asynchronous

##Main commands:

    project-explorer-open -- open the sidebar
    project-explorer-helm -- browse the file collection using helm

##Main key-bindings:

    "d"        Change directory
    "j"        Next line
    "k"        Previous line
    "g"        refresh
    "q"        hide sidebar
    "u"        go to parent directory
    "["        previous sibling
    "]"        next sibling
    "s"        isearch-forward
    "r"        isearch-backward
    "TAB"      toggle-folding
    "C-U TAB"  unfold descendants
    "RET"      toggle folding of visit file
    "f"        visit file or directory
    "w"        show the path of file at point, and copy it to clipboard

##Main customizable variables:

* pe/project-root-function

   Function that will locate the project root from the current
   `default-directory`

* pe/directory-files-function

   Indexing backend. One of `pe/get-directory-tree-async` (native, asynchronous,
   slow), `pe/get-directory-tree-simple`(native, synchronous) or
   `pe/get-directory-tree-find` (extrnal, asynchronus, best, but requires "find"
   with GNU extensions)

* pe/cache-enabled

   Whether to use caching

* pe/omit-regex

   Files and directories matching this regex won't be traversed

* pe/side

   Set to 'left or 'right, depending on which side you want the sidebar to appear

* pe/width

   The width of the sidebar when it first appears
