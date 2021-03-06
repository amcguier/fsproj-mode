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

;; Package-Requires: ((dash "2.6.0"))

;;; Code:

(require 'dash)
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

(defcustom Fsproj-menu-name-width 30
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

(defvar Fsproj-menu-project-file nil
  "The current project file.")
(make-variable-buffer-local 'Fsproj-menu-project-file)

(defvar Fsproj-menu-proj-doc nil
  "The current project file dom document.")
(make-variable-buffer-local 'Fsproj-menu-proj-doc)

(defvar Fsproj-menu-templates '(("lib" . fsharp-library-skeleton)
                                ("exe" . fsharp-console-skeleton))
  "An association list of supported F# project templates.")

(defvar Fsproj-extension "fsproj"
  "The filename extension used by Visual Studio F# project files.")

(defvar Fsproj-menu-compile-file "\\.fsi$\\|\\.fs$"
  "A regexp matching file names that have the Compile build action by default.")

(defvar Info-current-file) ; from info.el
(defvar Info-current-node) ; from info.el


(defvar file-status-in "+"
  "The string used to represent a file included in the project.")


(defvar file-status-out "-"
  "The string used to represent a file not included in the project.")


(defvar file-status-missing "!"
  "The string used to represent a file included in the project but missing from the file system.")


(defvar Fsproj-menu-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'Fsproj-menu-find-file)    
    (put 'Fsproj-menu-find-file :advertised-binding "\C-m")
    (define-key map "d" 'Fsproj-menu-delete-file)
    (define-key map "e" 'Fsproj-menu-find-file)
    (define-key map "f" 'Fsproj-menu-find-file)
    (define-key map "g" 'Fsproj-menu-refresh-buffer)
    (define-key map "m" 'Fsproj-menu-move)
    (define-key map "n" 'Fsproj-menu-new-file)
    (define-key map "o" 'Fsproj-menu-find-file-other-window)
    (define-key map "r" 'Fsproj-menu-rename-file)
    (define-key map "+" 'Fsproj-menu-add-file)
    (define-key map "-" 'Fsproj-menu-remove-file)
    (bindings--define-key menu-map [menu-bar immediate find-file-other-window]
      '(menu-item "Find in Other Window" Fsproj-menu-find-file-other-window
		  :help "Edit file at cursor in other window"))
    (bindings--define-key menu-map [new]
      '(menu-item "New" Fsproj-menu-new-file
                  :help "Add new file to project"))
    (bindings--define-key menu-map [add]
      '(menu-item "Add" Fsproj-menu-add-file
                  :help "Add file at cursor to project"))
    (bindings--define-key menu-map [remove]
      '(menu-item "Remove" Fsproj-menu-remove-file
                  :help "Remove file at cursor from project"))
    (bindings--define-key menu-map [delete]
      '(menu-item "Delete" Fsproj-menu-delete-file
                  :help "Delete file at cursor"))
    (bindings--define-key menu-map [menu-bar immediate find-file]
      '(menu-item "Open" Fsproj-menu-find-file
                  :help "Edit file at cursor"))
    (bindings--define-key menu-map [move]
      '(menu-item "Move" Fsproj-menu-move
                  :help "Move file at cursor to another position within the project"))
    (bindings--define-key menu-map [rename]
      '(menu-item "Rename" Fsproj-menu-rename-file
                  :help "Rename file at cursor"))
    (bindings--define-key menu-map [refresh]
      '(menu-item "Refresh" Fsproj-menu-refresh-buffer
                  :help "Refresh the project buffer"))
    (bindings--define-key menu-map [quit]
      '(menu-item "Quit" quit-window
                  :help "Remove the Fsproj Menu from the display"))
    map)
  "Local keymap for `Fsproj-menu-mode' buffers.")


(define-derived-mode Fsproj-menu-mode tabulated-list-mode "Fsproj Menu"
  "Major mode for Fsproj Menu buffers.
The Fsproj Menu is invoked by the commands \\[fsproj-menu] and
\\[fsproj-menu-other-window]. See `fsproj-menu' for a description
of its contents.

In Fsproj Menu mode, the following commands are defined:
\\<Fsproj-menu-mode-map>
Type \\[Fsproj-menu-new-file]\tAdd a new file to the project.
Type \\[Fsproj-menu-add-file]\tAdd file at cursor to the project.
Type \\[Fsproj-menu-remove-file]\tRemove file at cursor from the project.
Type \\[Fsproj-menu-delete-file]\tDelete file at cursor. 
Type \\[Fsproj-menu-find-file]\tOpen file at cursor in this window.
Type e\tOpen file at cursor in this window.
Type f\tOpen file at cursor in this window.
Type \\[Fsproj-menu-refresh-buffer]\tRefresh the project buffer.
Type \\[Fsproj-menu-move]\tMove file at cursor to another position within the project.
Type \\[Fsproj-menu-rename-file]\tRename file at cursor.
Type \\[Fsproj-menu-find-file-other-window]\tOpen file at cursor in another window.
Type \\[quit-window]\tQuit the project buffer.
"
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
    (message "Commands: RET, a, e, f, m, n, o, r, +, -, q to quit; ? for help.")))


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
  (message "Commands: RET, a, e, f, m, n, o, r, +, -, q to quit; ? for help."))


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


(defun move-file-item (doc from-position from-file-name to-position to-file-name)
  "Move the file in the itemGroup from fromIndex to toIndex."
  (let* ((root (dom-document-element doc))
         (new-child (car (dom-element-get-elements-by-attribute-value root "Include" from-file-name)))
         (item-group (dom-node-parent-node new-child))
         (ref-child (car (dom-element-get-elements-by-attribute-value root "Include" to-file-name))))
    (cond ((< from-position to-position)
           (dom-node-insert-before item-group new-child (dom-node-next-sibling ref-child)))
          ((> from-position to-position)
           (dom-node-insert-before item-group new-child ref-child)))))
 

(defun default-build-action (file-name)
  "Returns the default build action for the FILE-NAME."
  (if (string-match Fsproj-menu-compile-file file-name)
      'Compile
    'None))


(defun create-file-item (owner build-action file-name)
  "Returns a new BUILD-ACTION file item node"
  (let ((file-item (dom-document-create-element owner build-action))
        (include-attribute (dom-document-create-attribute owner 'Include)))
    (setf (dom-attr-value include-attribute) (file-name-nondirectory file-name)
          (dom-element-attributes file-item) (list include-attribute))
    file-item))


(defun create-item-group (owner)
  "Returns a new ItemGroup node."
  (dom-document-create-element (owner 'ItemGroup)))


(defun find-build-action-item-group (doc build-action)
  "Returns the ItemGroup for the given BUILD-ACTION, nil if none found."
  (let ((item (car (dom-document-get-elements-by-tag-name doc build-action))))
    (if item
        (dom-node-parent-node item)
      nil)))


(defun add-file-item (doc file-name)
  "Add the file to the DOC."
  (let* ((root (dom-document-element doc))
         (build-action (default-build-action file-name))
         (item (create-file-item doc build-action file-name))
         (item-group (find-build-action-item-group doc build-action)))
    (if item-group
        (dom-node-insert-before item-group item)
      (let (item-group (create-item-group))
        (dom-node-insert-before item-group item)
        (dom-node-insert-before root item-group)))))


(defun remove-file-item (doc file-name)
  "Remove the file from the DOC."
  (let* ((root (dom-document-element doc))
         (old-child (car (dom-element-get-elements-by-attribute-value root "Include" file-name)))
         (item-group (dom-node-parent-node old-child)))
    (if (> (length (dom-node-child-nodes item-group)) 1)        
        (dom-node-remove-child item-group old-child)
      (dom-node-remove-child doc item-group))))


(defun fsproj-rename-file (old-file-name new-file-name ok-if-already-exists)
  "Rename the OLD-FILE-NAME on disk to the NEW-FILE-NAME."
  (rename-file old-file-name new-file-name ok-if-already-exists)
  ;; Silently rename the visited file of any buffer visiting this file.
  (and (get-file-buffer old-file-name)
       (with-current-buffer (get-file-buffer old-file-name)
	 (set-visited-file-name new-file-name nil t))))


(defun include-attr-p (node)
  "Returns t if the NODE is an Include node"
  (eq 'Include (dom-node-name node)))


(defun fsproj-rename-file-item (doc old-file-name new-file-name)
  "Rename the OLD-FILE-NAME to NEW-FILE-NAME in the project DOC."
  (let* ((root (dom-document-element doc))
         (old-child (car (dom-element-get-elements-by-attribute-value root "Include" old-file-name)))
         (new-child (dom-node-clone-node old-child t))
         (item-group (dom-node-parent-node old-child))
         (include-attr (dom-document-create-attribute doc 'Include)))
    (setf (dom-attr-value include-attr) new-file-name
          (dom-element-attributes new-child) (list include-attr))
    (dom-node-replace-child item-group new-child old-child)))


(defun save-project-document (project-document project-file)
  "Save the project document to the project file."
  (when (and project-file (file-writable-p project-file))
    (with-temp-buffer
      (insert (dom-to-string-doc project-document))
      (write-region (point-min) (point-max) project-file))))


(defun fsproj-find-file-line-position (file-name)
  "Returns the buffer line position of the file-name."
  (let ((line-position (--find-index (string= (file-name-nondirectory file-name) (entry-id it)) tabulated-list-entries)))
    (when line-position
      (+ 1 line-position))))


(defun fsproj-goto-file (file-name)
  "Go to line describing file FILE-NAME in this project buffer."
  (goto-line (fsproj-find-file-line-position file-name)))


(defun fsproj-save-positions ()
  "Return current positions in the buffer and all windows with this project.
The positions have the form (BUFFER-POSITION WINDOW-POSITIONS).

BUFFER-POSITION is the point position in the current project buffer.
It has the form (BUFFER PROJECT-FILENAME BUFFER-POINT).

WINDOW-POSITIONS are current positions in all windows displaying
this project buffer.  The window positions have the form (WINDOW
PROJECT-FILENAME WINDOW-POINT)."
  (list
   (list (current-buffer) Fsproj-menu-project-file (point))
   (mapcar (lambda (w)
             (list w
                   (with-selected-window w
                     Fsproj-menu-project-file)
                   (window-point w)))
           (get-buffer-window-list nil 0 t))))


(defun fsproj-restore-positions (positions)
  "Restore POSITIONS saved with `fsproj-save-positions'."
  (let* ((buf-file-pos (nth 0 positions))
         (buffer (nth 0 buf-file-pos)))
    (unless (and (nth 1 buf-file-pos)
                 (fsproj-goto-file (nth 1 buf-file-pos)))
      (goto-char (nth 2 buf-file-pos)))
    (dolist (win-file-pos (nth 1 positions))
      ;; Ensure that window still displays the original buffer.
      (when (eq (window-buffer (nth 0 win-file-pos)) buffer)
        (with-selected-window (nth 0 win-file-pos)
          (unless (and (nth 1 win-file-pos)
                       (fsproj-goto-file (nth 1 win-file-pos)))
            (goto-char (nth 2 win-file-pos))))))))


(defun refresh-buffer (project-file)
  "Refresh the Fsproj-file-list buffer."
  (setq Fsproj-menu-proj-doc
        (dom-make-document-from-xml (car (xml-parse-file project-file))))
  (list-files--refresh project-file)
  (tabulated-list-print t))


(defun entry-vector-file-status (entry-vector)
  "Returns the file status for the ENTRY-VECTOR."
  (aref entry-vector 0))


(defun entry-vector-file-position (entry-vector)
  "Returns the file position for the ENTRY-VECTOR."
  (string-to-number (aref entry-vector 1)))


(defun entry-vector-file-build-action (entry-vector)
  "Returns the file build action for the ENTRY-VECTOR."
  (aref entry-vector 4))


(defun entry-id (entry)
  "Returns the file name for the ENTRY."
  (car entry))


(defun entry-vector (entry)
  "Returns the entry vector for the ENTRY."
  (cadr entry))


(defun entry-vector-compile-file-p (entry-vector)
  "Returns t if the ENTRY-VECTOR is for a compiled file."
  (string= (entry-vector-file-build-action entry-vector) "Compile"))


(defun entry-vector-included-file-p (entry-vector)
  "Returns t if the entry-vector is for a file included in the project."
  (not (string= (entry-vector-file-status entry-vector) file-status-out)))


(defun tabulated-list-get-entry-by-file-position (file-position)
  "Returns the file name at the INDEX in the tabulated list"
  (-first (lambda (entry)
            (eq file-position
                (entry-vector-file-position (entry-vector entry)))) tabulated-list-entries))


(defun tabulated-list-get-compile-file-count ()
  "Returns the number of files with the Compile build action in the tabulated list."
  (--count (entry-vector-compile-file-p (cadr it)) tabulated-list-entries))


(defun prompt-for-new-file-name-at-location (ask-mess default-location)
  "Ask for the name of the file to create.
Check to see if one exists already, and if so, ask for another name.
Asks the question ASK-MESS, and defaults to the using the location
DEFAULT-LOCATION.  Returns a list of a single string, full file name
with path."
  (let (file-name)
    (setq default-location (file-name-as-directory default-location))
    (while
        (progn
          (setq file-name (expand-file-name (read-file-name ask-mess default-location)))
          (if (not (string= (file-name-directory file-name) (expand-file-name default-location)))
              (setq ask-mess (format "New file must be in %s, please select again: " (expand-file-name default-location)))
            (setq ask-mess "That name is already in use, please use another name: "))
          (or
           (not (string= (file-name-directory file-name) (expand-file-name default-location)))
           (file-exists-p file-name))))
    file-name))

;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

(defun Fsproj-menu-new-file ()
  "Create and add a new file to the project."
  (interactive)
  (let ((file-name (prompt-for-new-file-name-at-location
                    "New file name: "
                    (file-name-directory Fsproj-menu-project-file))))
    (add-file-item Fsproj-menu-proj-doc (file-name-nondirectory file-name))
    (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file)
    (refresh-buffer Fsproj-menu-project-file)
    (find-file file-name)
    ;; TODO: put some templated content into the new buffer
    (save-buffer)))


(defun Fsproj-menu-add-file ()
  "Add the file at cursor to the project."
  (interactive)
  (let ((file-name (tabulated-list-get-id))
        (entry-vector (tabulated-list-get-entry)))
    (unless (entry-vector-included-file-p entry-vector)
      (add-file-item Fsproj-menu-proj-doc file-name)
      (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file)
      (refresh-buffer Fsproj-menu-project-file))))


(defun Fsproj-menu-rename-file ()
  "Rename the file at cursor."
  (interactive)
  (let* ((old-file-name (tabulated-list-get-id))
         (entry-vector (tabulated-list-get-entry))
         (new-file-name (prompt-for-new-file-name-at-location
                         (format "Rename %s to: " old-file-name)
                         (file-name-directory Fsproj-menu-project-file))))
    (fsproj-rename-file old-file-name new-file-name t)
    (when (entry-vector-included-file-p entry-vector)
      (fsproj-rename-file-item Fsproj-menu-proj-doc old-file-name (file-name-nondirectory new-file-name))
      (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file))
    (refresh-buffer Fsproj-menu-project-file)
    (fsproj-goto-file new-file-name)))


(defun Fsproj-menu-remove-file ()
  "Remove the file at cursor from the project."
  (interactive)
  (let ((file-name (tabulated-list-get-id))
        (entry-vector (tabulated-list-get-entry)))    
    (if (entry-vector-included-file-p entry-vector)
        (progn
          (remove-file-item Fsproj-menu-proj-doc file-name)
          (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file)
          (refresh-buffer Fsproj-menu-project-file))        
      (message "Cannot remove %s, can only remove files that are included in the project." file-name))))


(defun Fsproj-menu-delete-file ()
  "Delete the file at cursor from the disk and if it is included in the project then remove the file from the project too."
  (interactive)
  (let ((file-name (tabulated-list-get-id))
        (entry-vector (tabulated-list-get-entry)))
    (when (y-or-n-p (format "Delete %s?" file-name))
      (when (file-exists-p file-name)
        (delete-file file-name))
      (when (entry-vector-included-file-p entry-vector)
        (remove-file-item Fsproj-menu-proj-doc file-name)
        (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file))
      (refresh-buffer Fsproj-menu-project-file))))


(defun Fsproj-menu-get-file-for-visit ()
  "Get the current line's file name, with an error if file does not exist."
  (interactive)
  (let ((raw (tabulated-list-get-id))
        file-name)
    (if (null raw)
        (error "No file on this line"))
    (setq file-name (file-name-sans-versions raw t))
    (if (file-exists-p file-name)
        file-name
      (if (file-symlink-p file-name)
          (error "File is a symlink to a nonexistent target")
        ;(error "File no longer exists; type `g' to update Fsproj-menu
;buffer")
        file-name))))


(defun Fsproj-menu-find-file ()
  "In Fsproj-menu, visit the file named on this line."
  (interactive)
  (find-file (Fsproj-menu-get-file-for-visit)))


(defun Fsproj-menu-find-file-other-window ()
  "In Fsproj-menu, visit this file in another window."
  (interactive)
  (find-file-other-window (Fsproj-menu-get-file-for-visit)))


(defun Fsproj-menu-move-1 (to-position)
  "Move the file at cursor to TO-POSITION within the project."
  (interactive "nMove file to: ")
  (let* ((to-new-position (max (min to-position (tabulated-list-get-compile-file-count)) 1))
         (from-file-name (tabulated-list-get-id))
         (entry-vector (tabulated-list-get-entry))
         (item-group (file-item-group Fsproj-menu-item-tag-names Fsproj-menu-proj-doc))
         (from-position (entry-vector-file-position entry-vector))
         (to-file-name (entry-id (tabulated-list-get-entry-by-file-position to-new-position))))
    (unless (eq from-position to-new-position)
      (move-file-item Fsproj-menu-proj-doc from-position from-file-name to-new-position to-file-name)
      (save-project-document Fsproj-menu-proj-doc Fsproj-menu-project-file)
      (refresh-buffer Fsproj-menu-project-file))))


(defun Fsproj-menu-move ()
  "Move the file at cursor to another position within the project."
  (interactive)
  (let ((from-file-name (tabulated-list-get-id))
        (entry-vector (tabulated-list-get-entry)))    
    (if (entry-vector-compile-file-p entry-vector)
        (call-interactively 'Fsproj-menu-move-1)
      (message "Cannot move %s, can only move files with the Compile build action." from-file-name))))


(defun Fsproj-menu-refresh-buffer ()
  "Refresh the contents of the current Fsproj-menu buffer."
  (interactive)
  (refresh-buffer Fsproj-menu-project-file))


;;------------------------------------------------------------------------------


;;;###autoload
(defun list-files-noselect (proj-file &optional proj-only)
  "Create and return a Fsproj Menu buffer.
This is called by `fsproj-menu' and others as a subroutine.

If PROJ-ONLY is non-nil then show only files in project file,
otherwise show all files in the project file directory."
  (let ((old-buffer (current-buffer))
        (buffer (get-buffer-create (file-name-nondirectory proj-file))))
    (with-current-buffer buffer
      (Fsproj-menu-mode)
      (setq Fsproj-menu-proj-only
            (and proj-only (>= (prefix-numeric-value proj-only) 0)))
      (setq Fsproj-menu-project-file proj-file)
      (setq Fsproj-menu-proj-doc
            (dom-make-document-from-xml (car (xml-parse-file proj-file))))
      (list-files--refresh proj-file)
      (tabulated-list-print))
    buffer))


(defun file-number-p (e1 e2)
  "Returns non-nil if the first entry should sort before the second entry."
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
           `("No." 3 file-number-p :right-align t)
           `("File Name" ,name-width t)
           `("Size" ,size-width tabulated-list-entry-size-> :right-align t)
           '("Build Action" 18 t)
           '("Copy Action" 18 t))))
  (setq tabulated-list-sort-key (cons "No." nil))
  (setq tabulated-list-use-header-line Fsproj-menu-use-header-line)
  (if Fsproj-menu-proj-only
      (let ((project-file-list (project-item-entries Fsproj-menu-proj-doc)))
        (setq tabulated-list-entries project-file-list))
    (let* ((project-file-list (project-item-entries Fsproj-menu-proj-doc))
           (non-project-file-list
            (non-project-file-entries proj-file project-file-list)))
      (setq tabulated-list-entries (append non-project-file-list project-file-list))))
  (tabulated-list-init-header))


(defun file-entry (status index file-name build-action copy-action)
  "Returns an Fsproj-menu file entry for a file."
  (let* ((file-attrs (file-attributes file-name))
         (size (if (eq nil file-attrs) "" (number-to-string (nth 7 file-attrs))))
         (display-name (Fsproj-menu--pretty-name file-name)))
    (list file-name (vector status index display-name size build-action copy-action))))


(defun include-attr-value (project-item)
  "Returns the value of the Include attribute."
  (dom-node-value (car (dom-node-attributes project-item))))


(defun project-item-copy-action (item)
  "Returns the ITEM copy action."
  (let ((copy-action-node (car (dom-element-get-elements-by-tag-name item 'CopyToOutputDirectory))))
    (if copy-action-node
        (dom-node-text-content copy-action-node)
      "")))


(defun project-item-entry (compile-item-count item)
  "Returns a single item included in the project as a table entry."
  (let* ((node-name (dom-node-name item))
         (file-name (include-attr-value item))
         (file-index (if (eq node-name 'Compile) (number-to-string compile-item-count) ""))
         (file-status (if (file-exists-p file-name) file-status-in file-status-missing))
         (file-build-action (symbol-name node-name))
         (file-copy-action (project-item-copy-action item)))
    (file-entry file-status file-index file-name file-build-action file-copy-action)))


(defun project-item-entries (project)
  "Returns a list of items included in the project as table entries."
  (let ((compile-item-count 0)
        (item-entries))
    (dolist (item-group (dom-document-get-elements-by-tag-name project 'ItemGroup))
      (dolist (item (dom-node-child-nodes item-group))
        (let ((name (dom-node-name item)))
          (when (-contains? Fsproj-menu-item-tag-names name)
            (if (eq name 'Compile)
                (setq compile-item-count (incf compile-item-count)))
            (push (project-item-entry compile-item-count item) item-entries)))))
    (nreverse item-entries)))


(defun non-project-file-entries (proj-file project-file-entries)
  "Returns the list of non-project files in the project directory."
  (let ((dir-file-list (all-files-under-dir (file-name-directory proj-file) nil nil "^\\#\\|\\~$"))
        file-entries)
    (dolist (file dir-file-list)
      (if (not (or (file-directory-p file)
                   (assoc-string (file-name-nondirectory file) project-file-entries)
                   (string= "fsproj" (file-name-extension file))))
          (push (file-entry file-status-out "" (file-name-nondirectory file) "" "") file-entries)))
    (nreverse file-entries)))


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


(defvar Fsproj-menu-item-tag-names
  (list (intern "None")
        (intern "Compile")
        (intern "Content")
        (intern "EmbeddedResource"))
  "The tag names used for project file items.")


(defun file-item-group-p (file-item-tag-names itemGroup)
  "Returns t if the item group is the file item group otherwise nil"
  (-any?
   (lambda (child) (-contains? file-item-tag-names (dom-node-name child)))
   (dom-node-child-nodes itemGroup)))


(defun file-item-group (file-item-tag-names project-document)
  "If it exists then return the project ItemGroup containing the
project files otherwise return nil."
  (-first
   (lambda (itemGroup) (file-item-group-p file-item-tag-names itemGroup))
   (dom-document-get-elements-by-tag-name project-document 'ItemGroup)))


;;------------------------------------------------------------------------------
;; Misc.
;;------------------------------------------------------------------------------


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
                          (and exclude-regex exclude-regex-absolute-p
                               (not (string-match  exclude-regex  (expand-file-name sub head)))))
                  (setq files (append (list (expand-file-name sub head)) files)))))
          (if include-regexp
              (if (string-match include-regexp
                                (if include-regexp-absolute-path-p head (file-name-nondirectory head)))
                  (if exclude-regex
                      (if (not
                           (string-match exclude-regex
                                         (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                          (add-to-list 'matched-files head))
                    (add-to-list 'matched-files head)))
            (if exclude-regex
                (if (not (string-match exclude-regex
                                       (if exclude-regex-absolute-p head (file-name-nondirectory head))))
                    (add-to-list 'matched-files head))
              (add-to-list 'matched-files head))))))
    matched-files))


(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


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
;; DOM extensions
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


(defun dom-document-get-elements-by-attribute-value (doc attribute-name attribute-value)
  "Return a list of elements with an attribute with the given ATTRIBUTE-NAME and ATTRIBUTE-VALUE.
The special value \"*\" matches all attribute values."
  (dom-element-get-elements-by-attribute-value-1 (dom-document-element doc) attribute-name attribute-value))


(defun dom-element-get-elements-by-attribute-value (element attribute-name attribute-value)
  "Return a list of descendant elements of ELEMENT with the given ATTRIBUTE-NAME and ATTRIBUTE-VALUE.
The special value \"*\" matches all attribute values."
  (dom-element-get-elements-by-attribute-value-1 (dom-element-first-child element) attribute-name attribute-value))


(defun dom-element-get-elements-by-attribute-value-1 (element attribute-name attribute-value)
  "Return a list of elements with the given ATTRIBUTE-NAME and ATTRIBUTE-VALUE.
The elements are ELEMENT, its siblings and their descendants. This is used by
`dom-document-get-elements-by-attribute-value' and `dom-element-get-elements-by-attribute-value'."
  (let (stack result)
    (while element
      (when (dom-element-attribute-value-p element attribute-name attribute-value)
        (setq result (cons element result)))
      (setq element
            (cond ((dom-node-first-child element)
                   (when (dom-node-next-sibling element)
                     (push (dom-node-next-sibling element) stack))
                   (dom-node-first-child element))
                  ((dom-node-next-sibling element))
                  (t (pop stack)))))
    (nreverse result)))


(defun dom-element-attribute-value-p (element attribute-name attribute-value)
  "Returns t if ELEMENT has an attribute named ATTRIBUTE-NAME with a value ATTRIBUTE-VALUE.
The special value \"*\" matches all attribute values."
  (-any? (lambda (attribute)
           (and (string= (dom-node-name attribute) attribute-name)
                (or (string= attribute-value "*")
                    (string= (dom-node-value attribute) attribute-value))))
         (dom-node-attributes element)))


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
                 (dom-node-value (dom-node-last-child root2))))
        ))))


;; Test: non-project-file-entries
(eval-when-compile
  (when (file-readable-p "TestProject/TestProject.fsproj")
    
    ))


;;; fsproj-menu.el ends here
