# project-explorer
A tree project explorer.
![screenshot](https://github.com/sabof/project-explorer/raw/master/screenshot.png)

##Features:

    * Folding
    * Grouping of folders containing a single folder
    * I-search support
    * Occur support
    * Single key navigation keybindings

##Main commands:

    project-explorer-open -- open the sidebar
    project-explorer-helm -- browse the file collection using helm

##Main key-bindings:

    "d"        Change directory
    "+"        Create directory
    "-"        Delete directory
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
    "w"        copy to clipboard and show the path of file at point

##Main customizable variables:

    pe/side        Set to 'left or 'right, depending on which side you want the sidebar to appear
    pe/width       The width of the sidebar when it first appears
    pe/omit-regex  Files and directories matching this regex won't be traversed
