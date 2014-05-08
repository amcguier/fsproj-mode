;;; fsproj-menu.el -- Interface for viewing and manipulating F# projects

;; Copyright (C) 2014 Simon Cousins

;; Maintainer: Simon Cousins
;; Keywords: convenience
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details

;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Fsproj Menu is used to view, add, remove and order files within
;; a Visual Studio F# project. The entry point is M-x fsproj-menu.

;;; Code:

(require 'dom)
(require 'sgml-mode)
(require 'skeleton)
(require 'tabulated-list)

(defgroup Fsproj-menu nil
  "Show a menu of all of the files in a Visual Studio F# project."
  :group 'tools
  :group 'fsharp)

(defcustom Fsproj-menu-use-header-line t
  "If non-nil, use the header line to display Fsproj Menu column titles."
  :type 'boolean
  :group 'Fsproj-menu)

(defface fsproj-menu-file
  '((t (:weight bold)))
  "Face for file names in the Fsproj Menu."
  :group 'Fsproj-menu)
(put 'Fsproj-menu-file 'face-alias 'fsproj-menu-file)

(defcustom Fsproj-menu-name-width 19
  "Width of file name column in the Fsproj Menu."
  :type 'number
  :group 'Fsproj-menu)

(defcustom Fsproj-menu-size-width 7
  "Width of file size column in the Fsproj Menu."
  :type 'number
  :group 'Fsproj-menu)

(defcustom Fsproj-menu-mode-width 16
  "Width of mode name column in the Fsproj Menu."
  :type 'number
  :group 'Fsproj-menu)

(defvar Fsproj-menu-proj-only nil
  "Non-nil if the current Fsproj Menu lists only files in the
  project file. This is set by the prefix argument to
  `fsproj-menu' and related commands.")
(make-variable-buffer-local 'Fsproj-menu-proj-only)

(defvar Fsproj-menu-proj-doc nil
  "The current project file dom document.")
(make-variable-buffer-local 'Fsproj-menu-proj-doc)

(defvar Fsproj-menu-templates '(("lib" . fsharp-library-skeleton)
                                ("exe" . fsharp-console-skeleton))
  "An association list of supported F# project templates.")

(defvar Fsproj-extension "fsproj"
  "The filename extension used by Visual Studio F# project files.")

(defvar Info-current-file) ; from info.el
(defvar Info-current-node) ; from info.el


(defvar Fsproj-menu-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "m" 'Fsproj-menu-move)
    ;; TODO: define bindings
    (bindings--define-key menu-map [quit]
      '(menu-item "Quit" quit-window
                  :help "Remove the Fsproj Menu from the display"))
    (bindings--define-key menu-map [mv]
      '(menu-item "Move" Fsproj-menu-move
                  :help "Move the file on this line to another
    position in the project file")) map)
  "Local keymap for `Fsproj-menu-mode' buffers.")

(define-derived-mode Fsproj-menu-mode tabulated-list-mode "Fsproj Menu"
  "Major mode for Fsproj Menu buffers.
The Fsproj Menu is invoked by the commands \\[fsproj-menu] and
\\[fsproj-menu-other-window]. See `fsproj-menu' for a description
of its contents.

In Fsproj Menu mode, the following commands are defined:
\\<Fsproj-menu-mode-map>
\\[quit-window]    Remove the Fsproj Menu from the display.
\\[Fsproj-menu-move]    Move the current line's file."
  (add-hook 'tabulated-list-revert-hook 'list-files--refresh nil t))


(defun fsproj-start ()
  (or buffer-file-name dired-directory))


(defun fsproj-menu (&optional arg)
  "Switch to the Fsproj Menu.

By default, the Fsproj Menu lists all files in the same directory
as the project file. With prefix argument ARG, show only files
that are already in the project.

In the Fsproj Menu, the first column (denoted \"S\") shows the status
of the file where: \"+\" indicates a file in the project directory
that is included in the project, \"-\" indicates a file in the project
directory that is not included in the project and \"!\" indicates a file
that is included in the project but is not on disk.

The second column (denoted \"T\") shows the type of inclusion in the project
where: \"C\" indicates a compiled file, \"N\" indicates a non-compiled file
and \".\" indicates a file not included in the project.

The third column (denoted \"No.\") shows the position of a file included in the
project.

The remaining columns show the file name and the buffer size in
characters.

See `Fsproj-menu-mode' for the keybindings available the Fsproj
Menu."
  (interactive "P")
  (unless (fsharp-mode/find-fsproj (fsproj-start))
    (call-interactively 'create-fsproj-file))
  (when (fsharp-ac--valid-project-p (fsharp-mode/find-fsproj (fsproj-start)))
    (switch-to-buffer
     (list-files-noselect (fsharp-mode/find-fsproj (fsproj-start)) arg))
    (message "Commands: m, q to quit; ? for help.")))


(defun fsproj-menu-other-window (&optional arg)
  "Display the Fsproj Menu in another window.
See `fsproj-menu' for a description of the Fsproj Menu.

By default, all files are listed in the same directory as the project file.
With prefix argument
ARG, show only files that are in the project file."
  (interactive "P")
  (unless (fsharp-mode/find-fsproj (fsproj-start))
    (call-interactively 'create-fsproj-file))
  (unless (fsharp-ac--valid-project-p (fsharp-mode/find-fsproj (fsproj-start)))
    (switch-to-buffer-other-window
     (list-files-noselect (fsharp-mode/find-fsproj (fsproj-start)) arg)))
  (message "Commands: m, q to quit; ? for help."))


(defun create-fsproj-file (file-name template)
  "Creates an F# project file from a supported template.
See `Fsproj-menu-templates' for the list of supported templates."
  (interactive
   (list
    (read-file-name "Fsproj file name: ")
    (completing-read "Fsproj type: " Fsproj-menu-templates
                     nil t "" nil (car (car Fsproj-menu-templates)))))
  (unless (string= (file-name-extension file-name) Fsproj-extension)()
      (setq file-name (concat file-name "." Fsproj-extension)))
  (with-temp-file file-name
    (sgml-mode)
    (funcall (cdr (assoc template Fsproj-menu-templates)))
    file-name))


;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------


(defun Fsproj-menu-move (toIndex)
  "Move the current line's file to another position within the project."
  (interactive "nMove file to: ")
  ;; TODO: check file is included in the project
  (let* ((file-name (tabulated-list-get-id))
         (entry (tabulated-list-get-entry))
         (fromIndex (string-to-number (aref entry 2)))
         (file-status (aref entry 0)))
    (message "MOVE %s%s from %d to %d" file-status file-name fromIndex toIndex)))


;;------------------------------------------------------------------------------


;;;###autoload
(defun list-files-noselect (proj-file &optional proj-only)
  "Create and return a Fsproj Menu buffer.
This is called by `fsproj-menu' and others as a subroutine.

If PROJ-ONLY is non-nil then show only files in project file,
otherwise show all files in the project file directory."
  (let ((old-buffer (current-buffer))
        (buffer (get-buffer-create "*Fsproj File List*")))
    (with-current-buffer buffer
      (Fsproj-menu-mode)
      (setq Fsproj-menu-proj-only
            (and proj-only (>= (prefix-numeric-value proj-only) 0)))
      (setq Fsproj-menu-proj-doc
            (dom-make-document-from-xml (car (xml-parse-file proj-file))))
      (list-files--refresh proj-file)
      (tabulated-list-print))
    buffer))


(defun file-number-p (e1 e2)
  "Returns non-nil is the first entry should sort before the second entry."
  (let ((n1 (elt (cadr e1) 2))
        (n2 (elt (cadr e2) 2)))
    (cond ((and (string= "." n1) (string= "." n2))
           t)
          ((and (string= "." n1) (not (string= "." n2)))
           nil)
          ((and (not (string= "." n1)) (string= "." n2))
           t)
          (t
           (let ((number1 (string-to-number n1))
                 (number2 (string-to-number n2)))
             (< number1 number2))))))


(defun list-files--refresh (proj-file)
  ;; Set up `tabulated-list-format'.
  (let ((name-width Fsproj-menu-name-width)
        (size-width Fsproj-menu-size-width))
    (setq tabulated-list-format
          (vector
           '("S" 1 t)
           '("T" 1 t)
           `("No." 3 file-number-p :right-align t)
           `("File" ,name-width t)
           `("Size" ,size-width tabulated-list-entry-size-> :right-align t))))
  (setq tabulated-list-sort-key (cons "No." nil))
  (setq tabulated-list-use-header-line Fsproj-menu-use-header-line)
  (if Fsproj-menu-proj-only
      (let ((project-file-list (project-file-entries Fsproj-menu-proj-doc)))
        (setq tabulated-list-entries project-file-list))
    (let* ((project-file-list (project-file-entries Fsproj-menu-proj-doc))
           (non-project-file-list
            (non-project-file-entries proj-file project-file-list)))
      (setq tabulated-list-entries (append non-project-file-list project-file-list))))
  (tabulated-list-init-header))


(defun file-entry (file-name file-status file-type file-index)
  "Returns an Fsproj-menu file entry for a file."
  (let ((file-attrs (file-attributes file-name)))
    (list file-name
          (vector
           ;; file status
           file-status
           ;; file type: compiled or non-compiled
           file-type
           ;; file index
           file-index
           ;; file name
           (Fsproj-menu--pretty-name file-name)
           ;; file size
           (if (eq nil file-attrs) "" (number-to-string (nth 7 file-attrs)))))))


(defun include-attr-value (node)
  "Returns the value of the Include attribute."
  (dom-node-value (car (dom-node-attributes node))))


(defun project-file-entries (project)
  "Returns a list of the files included in the project."
  (let ((counter 0)
        (entries))
    (dolist (itemGroup (dom-document-get-elements-by-tag-name project 'ItemGroup))
      (dolist (item (dom-node-child-nodes itemGroup))
        (let ((name (dom-node-name item)))
          (cond ((eq name 'Compile)
                 (setq counter (incf counter))
                 (let ((file-name (include-attr-value item)))
                   (push (file-entry
                          file-name
                          (if (file-exists-p file-name) "+" "!")
                          "C"
                          (number-to-string counter)) entries)))
                ((eq name 'None)
                 (setq counter (incf counter))
                 (let ((file-name (include-attr-value item)))
                   (push (file-entry
                          file-name
                          (if (file-exists-p file-name) "+" "!")
                          "N"
                          (number-to-string counter)) entries)))
                ))))
    (nreverse entries)))


(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun non-project-file-entries (proj-file project-file-entries)
  "Returns the list of non-project files in the project directory."
  (let* ((dir-file-list (all-files-under-dir (file-name-directory proj-file) nil nil "^\\#\\|\\~$"))
         entries)
    (dolist (file dir-file-list)
      (if (not (or (file-directory-p file)
                   (assoc-string (file-name-nondirectory file) project-file-entries)
                   (string= "fsproj" (file-name-extension file))))
          (push (file-entry (file-name-nondirectory file) "-" "." ".") entries)))
    (nreverse entries)))


;;;###autoload
(defun all-files-under-dir
  (dir &optional include-regexp  include-regexp-absolute-path-p exclude-regex exclude-regex-absolute-p)
  "Return all files matched `include-regexp' under directory `dir'.
if `include-regexp' is nil ,return all.
when `include-regexp-absolute-path-p' is nil or omited ,filename is used to match `include-regexp'
when `include-regexp-absolute-path-p' is t then full file path is used to match `include-regexp'
when `exclude-regexp-absolute-path-p' is nil or omited ,filename is used to match `exclude-regexp'
when `exclude-regexp-absolute-path-p' is t then full file path is used to match `exclude-regexp'
"
  (let((files (list dir))  matched-files head)
    (while (> (length files) 0)
      (setq head (pop files))
      (when (file-readable-p head)
        (if (and (eq dir head) (file-directory-p head))
            (dolist (sub (directory-files head))
              (when  (not (string-match "^\\.$\\|^\\.\\.$" sub))
                (when (or (not exclude-regex)
                          (and exclude-regex (not exclude-regex-absolute-p))
                          (and exclude-regex exclude-regex-absolute-p  (not (string-match  exclude-regex  (expand-file-name sub head)))))
                  (setq files (append (list (expand-file-name sub head)) files)))))
          (if include-regexp
              (if (string-match include-regexp (if include-regexp-absolute-path-p head (file-name-nondirectory head)))
                  (if exclude-regex
                      (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                          (add-to-list 'matched-files head))
                    (add-to-list 'matched-files head)))
            (if exclude-regex
                (if (not (string-match exclude-regex (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                    (add-to-list 'matched-files head))
              (add-to-list 'matched-files head))))))
    matched-files))


(defun compiled-file-p (compiled-file-list afile)
  (member (file-name-nondirectory afile) compiled-file-list))

(defun tabulated-list-entry-size-> (entry1 entry2)
  (> (string-to-number (aref (cadr entry1) 4))
     (string-to-number (aref (cadr entry2) 4))))

(defun Fsproj-menu--pretty-name (name)
  (propertize name
              'font-lock-face 'buffer-menu-buffer
              'mouse-face 'highlight))

(defun Fsproj-menu--pretty-file-name (file)
  (cond (file
         (abbreviate-file-name file))
        ((and (boundp 'list-buffers-directory)
              list-buffers-directory)
         list-buffers-directory)
        ((eq major-mode 'Info-mode)
         (Fsproj-menu-info-node-description Info-current-file))
        (t "")))


(defun Fsproj-menu-info-node-description (file)
  (cond
   ((equal file "dir") "*Info Directory*")
   ((eq file 'apropos) "*Info Apropos*")
   ((eq file 'history) "*Info History*")
   ((eq file 'toc)     "*Info TOC*")
   ((not (stringp file)) "") ; Avoid errors
   (t
    (concat "(" (file-name-nondirectory file) ") " Info-current-node))))


;;------------------------------------------------------------------------------
;; Project Templates
;;------------------------------------------------------------------------------


(define-skeleton fsharp-console-skeleton
  "F# console project skeleton."
  ""
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  "<Project DefaultTargets=\"Build\" ToolsVersion=\"4.0\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">\n"
  > "<PropertyGroup>\n"
  > "<Configuration Condition=\" '$(Configuration)' == '' \">Debug</Configuration>\n"
  > "<Platform Condition=\" '$(Platform)' == '' \">AnyCPU</Platform>\n"
  > "<ProductVersion>10.0.0</ProductVersion>\n"
  > "<SchemaVersion>2.0</SchemaVersion>\n"
  > "<ProjectGuid>"
  (new-guid)
  "</ProjectGuid>\n"
  > "<OutputType>Exe</OutputType>\n"
  > "<RootNamespace>"
  (setq v1 (skeleton-read "Namespace: "))
  "</RootNamespace>"  > \n
  > "<AssemblyName>"
  (setq v2 (skeleton-read "Assembly name: ")) | "default-assembly-name"
  "</AssemblyName>\n"
  "</PropertyGroup>" > \n
  "<PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' \">\n"
  > "<DebugSymbols>true</DebugSymbols>\n"
  > "<DebugType>full</DebugType>\n"
  > "<OutputPath>bin\\Debug</OutputPath>\n"
  > "<DefineConstants>DEBUG</DefineConstants>\n"
  > "<ErrorReport>prompt</ErrorReport>\n"
  > "<PlatformTarget>"
  "</PlatformTarget>\n"
  > "<Externalconsole>true</Externalconsole>\n"
  "</PropertyGroup>" > \n
  > "<PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Release|AnyCPU' \">\n"
  > "<DebugSymbols>true</DebugSymbols>\n"
  > "<DebugType>pdbonly</DebugType>\n"
  > "<Optimize>true</Optimize>\n"
  > "<OutputPath>bin\\Release</OutputPath>\n"
  > "<ErrorReport>prompt</ErrorReport>\n"
  > "<PlatformTarget>"
  "</PlatformTarget>\n"
  > "<Externalconsole>true</Externalconsole>\n"
  > "<Tailcalls>true</Tailcalls>\n"
  "</PropertyGroup>" > \n
  > "<ItemGroup>\n"
  > "<Reference Include=\"mscorlib\" />\n"
  > "<Reference Include=\"FSharp.Core, Version=4.3.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" />\n"
  > "<Reference Include=\"System\" />\n"
  > "<Reference Include=\"System.Core\" />\n"
  > "<Reference Include=\"System.Numerics\" />\n"
  "</ItemGroup>" > \n
  > "<ItemGroup>\n"
  > "<Compile Include=\"AssemblyInfo.fs\" />\n"
  > "<Compile Include=\"Program.fs\" />\n"
  "</ItemGroup>" > \n
  > "<Import Project=\"$(MSBuildExtensionsPath32)\\..\\Microsoft SDKs\\F#\\3.1\\Framework\\v4.0\\Microsoft.FSharp.Targets\" />\n"
  "</Project>" > \n \n
  )

(define-skeleton fsharp-library-skeleton
  "F# library project skeleton."
  ""
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  "<Project DefaultTargets=\"Build\" ToolsVersion=\"4.0\" xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">\n"
  > "<PropertyGroup>\n"
  > "<Configuration Condition=\" '$(Configuration)' == '' \">Debug</Configuration>\n"
  > "<Platform Condition=\" '$(Platform)' == '' \">AnyCPU</Platform>\n"
  > "<ProductVersion>10.0.0</ProductVersion>\n"
  > "<SchemaVersion>2.0</SchemaVersion>\n"
  > "<ProjectGuid>"
  (new-guid)
  "</ProjectGuid>\n"
  > "<OutputType>Library</OutputType>\n"
  > "<RootNamespace>"
  (setq v1 (skeleton-read "Namespace: "))
  "</RootNamespace>\n"
  > "<AssemblyName>"
  (setq v2 (skeleton-read "Assembly name: ")) | "default-assembly-name"
  "</AssemblyName>\n"
  "</PropertyGroup>"  > \n
  > "<PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' \">\n"
  > "<DebugSymbols>true</DebugSymbols>\n"
  > "<DebugType>full</DebugType>\n"
  > "<OutputPath>bin\\Debug</OutputPath>\n"
  > "<DefineConstants>DEBUG</DefineConstants>\n"
  > "<ErrorReport>prompt</ErrorReport>\n"
  > "<PlatformTarget>"
  "</PlatformTarget>\n"
  > "<ConsolePause>false</ConsolePause>\n"
  "</PropertyGroup>" > \n
  > "<PropertyGroup Condition=\" '$(Configuration)|$(Platform)' == 'Release|AnyCPU' \">\n"
  > "<DebugSymbols>true</DebugSymbols>\n"
  > "<DebugType>pdbonly</DebugType>\n"
  > "<Optimize>true</Optimize>\n"
  > "<OutputPath>bin\\Release</OutputPath>\n"
  > "<ErrorReport>prompt</ErrorReport>\n"
  > "<PlatformTarget>"
  "</PlatformTarget>\n"
  > "<ConsolePause>false</ConsolePause>\n"
  > "<Tailcalls>true</Tailcalls>\n"
  "</PropertyGroup>" > \n
  > "<ItemGroup>\n"
  > "<Reference Include=\"mscorlib\" />\n"
  > "<Reference Include=\"FSharp.Core, Version=4.3.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a\" />\n"
  > "<Reference Include=\"System\" />\n"
  > "<Reference Include=\"System.Core\" />\n"
  > "<Reference Include=\"System.Numerics\" />\n"
  "</ItemGroup>" > \n
  > "<ItemGroup>\n"
  > "<Compile Include=\"AssemblyInfo.fs\" />\n"
  > "<Compile Include=\"Component1.fs\" />\n"
  > "<Compile Include=\"Script.fsx\" />\n"
  "</ItemGroup>" > \n
  > "<Import Project=\"$(MSBuildExtensionsPath32)\\..\\Microsoft SDKs\\F#\\3.1\\Framework\\v4.0\\Microsoft.FSharp.Targets\" />\n"
  "</Project>" > \n \n
  )


;;------------------------------------------------------------------------------
;; GUID generation
;;------------------------------------------------------------------------------


(defun new-guid ()
  "Creates a new GUID."
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (format "{%s-%s-4%s-a%s-%s}"
            (substring myStr 0 8)
            (substring myStr 8 12)
            (substring myStr 13 16)
            (substring myStr 17 20)
            (substring myStr 20 32))))


;;------------------------------------------------------------------------------
;; DOM to string
;;------------------------------------------------------------------------------


(defun dom-to-string-doc (doc)
  "Convert a DOM document to an XML string."
  (let ((result "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        (root (dom-document-element doc)))
    (setq result (concat result (dom-to-string-node root)))))


(defun dom-to-string-nodes (nodes)
  "Convert a DOM node list to an XML string."
  (let ((result))
    (dolist (node nodes result)
      (setq result (concat result (dom-to-string-node node))))))


(defun dom-to-string-node (node)
  "Convert a DOM node to an XML string."
  (let ((node-name (symbol-name (dom-node-name node)))
        (child-nodes (dom-node-child-nodes node)))
    (if (dom-text-p node)
        (dom-node-value node)
      (concat
       "<" node-name (dom-to-string-attributes node) ">"
       (dom-to-string-nodes child-nodes)
       "</" node-name ">"))))


(defun dom-to-string-attributes (node)
  "Convert an attribute list to an XML attributes string."
  (let ((result)
        (attributes (dom-node-attributes node)))
    (dolist (attribute attributes result)
      (setq result (concat result (dom-to-string-attribute attribute))))))


(defun dom-to-string-attribute (attribute)
  "Convert an attribute to an XML attribute string."
  (let ((name (symbol-name (dom-node-name attribute)))
        (value (dom-node-value attribute)))
    (concat " " name "=\"" value "\"")))


;;------------------------------------------------------------------------------
;; Project - stolen from fsharp-mode
;;------------------------------------------------------------------------------


(defun fsharp-mode/find-fsproj (dir-or-file)
  (fsharp-mode-search-upwards (rx (0+ nonl) ".fsproj" eol)
                              (file-name-directory dir-or-file)))


(defun fsharp-mode-search-upwards (regex dir)
  (when dir
    (or (car-safe (directory-files dir 'full regex))
        (fsharp-mode-search-upwards regex (fsharp-mode-parent-dir dir)))))


(defun fsharp-mode-parent-dir (dir)
  (let ((p (file-name-directory (directory-file-name dir))))
    (unless (equal p dir)
      p)))


(defun fsharp-ac--valid-project-p (file)
  (and file
       (file-exists-p file)
       (string-match-p (rx "." "fsproj" eol) file)))


;;------------------------------------------------------------------------------
;; Unit Tests
;;------------------------------------------------------------------------------


;; Test: dom-to-string
(eval-when-compile
  (when (file-readable-p "TestProject/TestProject.fsproj")
    (let* ((doc1 (dom-make-document-from-xml
                  (car (xml-parse-file "TestProject/TestProject.fsproj"))))
           (root1 (dom-document-element doc1)))
      (let* ((xml (dom-to-string-doc doc1))
             (doc2 (with-temp-buffer
                     (insert xml)
                     (dom-make-document-from-xml
                      (car (xml-parse-region (point-min) (point-max))))))
             (root2 (dom-document-element doc2)))
        (assert (not (eq root1 root2)))
        (assert (not (eq
                      (dom-node-attributes root1)
                      (dom-node-attributes root2))))
        (assert (eq
                 (dom-node-name (car (dom-node-attributes root1)))
                 (dom-node-name (car (dom-node-attributes root2)))))
        (assert (not (eq
                      (dom-node-value (car (dom-node-attributes root1)))
                      (dom-node-value (car (dom-node-attributes root2))))))
        (assert (equal
                 (dom-node-value (car (dom-node-attributes root1)))
                 (dom-node-value (car (dom-node-attributes root2)))))
        (assert (equal
                 (dom-node-name (dom-node-last-child root1))
                 (dom-node-name (dom-node-last-child root2))))
        (assert (equal
                 (dom-node-value (dom-node-last-child root1))
                 (dom-node-value (dom-node-last-child root2))))))))

;; Test: non-project-file-entries
(eval-when-compile
  (when (file-readable-p "TestProject/TestProject.fsproj")
    (let* ((doc (dom-make-document-from-xml (car (xml-parse-file "TestProject/TestProject.fsproj"))))
           (proj-entries (project-file-entries doc))
           (non-proj-entries (non-project-file-entries "TestProject/TestProject.fsproj" proj-entries)))
      (assert (eq 1 (length non-proj-entries)))
      (assert (string= "Foo.txt" (caar non-proj-entries))))))


;;; fsproj-menu.el ends here
