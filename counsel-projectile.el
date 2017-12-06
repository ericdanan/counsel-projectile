;;; counsel-projectile.el --- Ivy integration for Projectile

;; Copyright (C) 2016 Eric Danan

;; Author: Eric Danan
;; URL: https://github.com/ericdanan/counsel-projectile
;; Created: 2016-04-11
;; Keywords: project, convenience
;; Version: 0.1
;; Package-Requires: ((counsel "0.8.0") (projectile "0.14.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Projectile has native support for using ivy as its completion
;; system.  Counsel-projectile provides further ivy integration into
;; projectile by taking advantage of ivy's mechanism to select from a
;; list of actions and/or apply an action without leaving the
;; comlpetion session.  It is inspired by helm-projectile.  See the
;; README for more details.
;;
;;; Code:

(require 'counsel)
(require 'projectile)

;;; customization

(defgroup counsel-projectile nil
  "Ivy integration for Projectile."
  :group 'ivy
  :group 'projectile)
                       
(defcustom counsel-projectile-remove-current-buffer nil
  "Non-nil if current buffer should be removed from the
candidates list of `counsel-projectile-switch-to-buffer' and
`counsel-projectile'."
  :type 'boolean
  :group 'counsel-projectile)

(defcustom counsel-projectile-remove-current-project nil
  "Non-nil if current project should be removed from the
candidates list of `counsel-projectile-switch-project'."
  :type 'boolean
  :group 'counsel-projectile)

(defcustom counsel-projectile-grep-initial-input nil
  "Initial minibuffer input for `counsel-projectile-grep'.  If
non-nil, it should be a Lisp expression whose evaluation yields
the initial input string.

Note that you can always insert the value
of `(ivy-thing-at-point)' by hitting \"M-n\" in the minibuffer."
  :type '(choice
          (const :tag "None" nil)
          (const :tag "Symbol at point (generic)" '(thing-at-point 'symbol t))
          (const :tag "Symbol or selection at point (projectile)" '(projectile-symbol-or-selection-at-point))
          (const :tag "Thing at point (ivy)" '(ivy-thing-at-point))
          (sexp  :tag "Custom expression"))
  :group 'counsel-projectile)

(defcustom counsel-projectile-ag-initial-input nil
  "Initial minibuffer input for `counsel-projectile-ag'.  If
non-nil, it should be a Lisp expression whose evaluation yields
the initial input string.

Note that you can always insert the value
of `(ivy-thing-at-point)' by hitting \"M-n\" in the minibuffer."
  :type '(choice
          (const :tag "None" nil)
          (const :tag "Symbol at point (generic)" '(thing-at-point 'symbol t))
          (const :tag "Symbol or selection at point (projectile)" '(projectile-symbol-or-selection-at-point))
          (const :tag "Thing at point (ivy)" '(ivy-thing-at-point))
          (sexp  :tag "Custom expression"))
  :group 'counsel-projectile)

(defcustom counsel-projectile-rg-initial-input nil
  "Initial minibuffer input for `counsel-projectile-rg'.  If
non-nil, it should be a Lisp expression whose evaluation yields
the initial input string.

Note that you can always insert the value
of `(ivy-thing-at-point)' by hitting \"M-n\" in the minibuffer."
  :type '(choice
          (const :tag "None" nil)
          (const :tag "Symbol at point (generic)" '(thing-at-point 'symbol t))
          (const :tag "Symbol or selection at point (projectile)" '(projectile-symbol-or-selection-at-point))
          (const :tag "Thing at point (ivy)" '(ivy-thing-at-point))
          (sexp  :tag "Custom expression"))
  :group 'counsel-projectile)

(defcustom counsel-projectile-org-capture-templates nil
  "Templates for the creation of new entries with `counsel-projectile-org-capture'.

The format is the same as in `org-capture-templates', except that
in all strings of in an entry's target slot, all instances of
\"${root}\" and \"${name}\" are replaced with the current project
root and name, respectively.

Examples of template targets:

    \(file+headline \"${root}/${name}.org}\" \"Notes\"\)
    \(file+olp \"~/notes.org\" \"${root}\" \"Todos\"\)

The former points to headline \"Notes\" in file
\"<project-name>.org\" in the project root directory (one file per project), whereas the latter points to
outline path \"<project-root>/Todos\" in file
\"~/notes.org\" (same file for all projects)."
  :type ;; copied from `org-capture-templates'
  (let ((file-variants '(choice :tag "Filename       "
				(file :tag "Literal")
				(function :tag "Function")
				(variable :tag "Variable")
				(sexp :tag "Form"))))
    `(repeat
      (choice :value ("" "" entry (file "~/org/notes.org") "")
              (list :tag "Multikey description"
                    (string :tag "Keys       ")
                    (string :tag "Description"))
              (list :tag "Template entry"
                    (string :tag "Keys           ")
                    (string :tag "Description    ")
                    (choice :tag "Capture Type   " :value entry
                            (const :tag "Org entry" entry)
                            (const :tag "Plain list item" item)
                            (const :tag "Checkbox item" checkitem)
                            (const :tag "Plain text" plain)
                            (const :tag "Table line" table-line))
                    (choice :tag "Target location"
                            (list :tag "File"
                                  (const :format "" file)
                                  ,file-variants)
                            (list :tag "ID"
                                  (const :format "" id)
                                  (string :tag "  ID"))
                            (list :tag "File & Headline"
                                  (const :format "" file+headline)
                                  ,file-variants
                                  (string :tag "  Headline"))
                            (list :tag "File & Outline path"
                                  (const :format "" file+olp)
                                  ,file-variants
                                  (repeat :tag "Outline path" :inline t
                                          (string :tag "Headline")))
                            (list :tag "File & Regexp"
                                  (const :format "" file+regexp)
                                  ,file-variants
                                  (regexp :tag "  Regexp"))
                            (list :tag "File [ & Outline path ] & Date tree"
                                  (const :format "" file+olp+datetree)
                                  ,file-variants
                                  (option (repeat :tag "Outline path" :inline t
                                                  (string :tag "Headline"))))
                            (list :tag "File & function"
                                  (const :format "" file+function)
                                  ,file-variants
                                  (sexp :tag "  Function"))
                            (list :tag "Current clocking task"
                                  (const :format "" clock))
                            (list :tag "Function"
                                  (const :format "" function)
                                  (sexp :tag "  Function")))
                    (choice :tag "Template       "
                            (string)
                            (list :tag "File"
                                  (const :format "" file)
                                  (file :tag "Template file"))
                            (list :tag "Function"
                                  (const :format "" function)
                                  (function :tag "Template function")))
                    (plist :inline t
                           ;; Give the most common options as checkboxes
                           :options (((const :format "%v " :prepend) (const t))
                                     ((const :format "%v " :immediate-finish) (const t))
                                     ((const :format "%v " :jump-to-captured) (const t))
                                     ((const :format "%v " :empty-lines) (const 1))
                                     ((const :format "%v " :empty-lines-before) (const 1))
                                     ((const :format "%v " :empty-lines-after) (const 1))
                                     ((const :format "%v " :clock-in) (const t))
                                     ((const :format "%v " :clock-keep) (const t))
                                     ((const :format "%v " :clock-resume) (const t))
                                     ((const :format "%v " :time-prompt) (const t))
                                     ((const :format "%v " :tree-type) (const week))
                                     ((const :format "%v " :unnarrowed) (const t))
                                     ((const :format "%v " :table-line-pos) (string))
                                     ((const :format "%v " :kill-buffer) (const t))))))))
  :group 'counsel-projectile)

(defcustom counsel-projectile-org-capture-templates-contexts nil
  "Alist of capture templates and valid contexts for `counsel-projectile-org-capture'.

The format is the same as in `org-capture-templates-contexts'."
  :type ;; copied from `org-capture-target-templates'
  '(repeat (list :tag "Rule"
                 (string :tag "        Capture key")
                 (string :tag "Replace by template")
                 (repeat :tag "Available when"
                         (choice
                          (cons :tag "Condition"
                                (choice
                                 (const :tag "In file" in-file)
                                 (const :tag "Not in file" not-in-file)
                                 (const :tag "In buffer" in-buffer)
                                 (const :tag "Not in buffer" not-in-buffer)
                                 (const :tag "In mode" in-mode)
                                 (const :tag "Not in mode" not-in-mode))
                                (regexp))
                          (function :tag "Custom function")))))
  :group 'counsel-projectile)

(defcustom counsel-projectile-find-file-actions
  '(("j" counsel-projectile-find-file-action-other-window
     "other window")
    ("x" counsel-projectile-find-file-action-extern
     "open externally")
    ("r" counsel-projectile-find-file-action-root
     "open as root"))
  "List of actions for `counsel-projecile-find-file'.

Each action is made of:

- a key (one-letter string, avoiding \"o\" which is reserved for
  the default action) to call the action, a function of one
- variable (the selected candidate) to execute the action, a
- name (string) for the action.

If you modify this variable outside the Customize interface and
after loading counsel-projectile, then you should evaluate
 
    (ivy-set-actions 
     'counsel-projectile-find-file
     counsel-projectile-find-file-actions)

afterwards to apply your changes."
  :type '(repeat
          (list :tag "Action"
                (string   :tag "     key")
                (function :tag "function")
                (string   :tag "    name")))
  :set (lambda (sym val)
         (set sym val)
         (ivy-set-actions 'counsel-projectile-find-file val))
  :group 'counsel-projectile)

(defcustom counsel-projectile-find-dir-actions
  '(("j" counsel-projectile-find-dir-action-other-window
     "other window"))
  "List of actions for `counsel-projecile-find-dir'.

Each action is made of:

- a key (one-letter string, avoiding \"o\" which is reserved for
  the default action) to call the action, a function of one
- variable (the selected candidate) to execute the action, a
- name (string) for the action.

If you modify this variable outside the Customize interface and
after loading counsel-projectile, then you should evaluate
 
    (ivy-set-actions 
     'counsel-projectile-find-dir
     counsel-projectile-find-dir-actions)

afterwards to apply your changes."
  :type '(repeat
          (list :tag "Action"
                (string   :tag "     key")
                (function :tag "function")
                (string   :tag "    name")))
  :set (lambda (sym val)
         (set sym val)
         (ivy-set-actions 'counsel-projectile-find-dir val))
  :group 'counsel-projectile)

(defcustom counsel-projectile-switch-to-buffer-actions
  '(("j" switch-to-buffer-other-window
     "other window"))
  "List of actions for `counsel-projecile-switch-to-buffer'.

Each action is made of:

- a key (one-letter string, avoiding \"o\" which is reserved for
  the default action) to call the action, a function of one
- variable (the selected candidate) to execute the action, a
- name (string) for the action.

If you modify this variable outside the Customize interface and
after loading counsel-projectile, then you should evaluate
 
    (ivy-set-actions 
     'counsel-projectile-switch-to-buffer
     counsel-projectile-switch-to-buffer-actions)

afterwards to apply your changes."
  :type '(repeat
          (list :tag "Action"
                (string   :tag "     key")
                (function :tag "function")
                (string   :tag "    name")))
  :set (lambda (sym val)
         (set sym val)
         (ivy-set-actions 'counsel-projectile-switch-to-buffer val))
  :group 'counsel-projectile)

(defcustom counsel-projectile-switch-project-actions
  '(("f" counsel-projectile-switch-project-action-find-file
     "find file")
    ("F" counsel-projectile-switch-project-action-find-file-manually
     "find file manually")
    ("d" counsel-projectile-switch-project-action-find-dir
     "find directory")
    ("b" counsel-projectile-switch-project-action-switch-to-buffer
     "switch to buffer")
    ("s" counsel-projectile-switch-project-action-save-all-buffers
     "save all buffers")
    ("k" counsel-projectile-switch-project-action-kill-buffers
     "kill all buffers")
    ("r" counsel-projectile-switch-project-action-remove-known-project
     "remove from known projects")
    ("l" counsel-projectile-switch-project-action-edit-dir-locals
     "edit dir-locals")
    ("g" counsel-projectile-switch-project-action-vc
     "open in vc-dir / magit / monky")
    ("e" counsel-projectile-switch-project-action-run-eshell
     "start eshell")
    ("G" counsel-projectile-switch-project-action-grep
     "search with grep")
    ("a" counsel-projectile-switch-project-action-ag
     "search with ag")
    ("R" counsel-projectile-switch-project-action-rg
     "search with rg")
    ("c" counsel-projectile-switch-project-action-org-capture
     "org-capture"))
  "List of actions for `counsel-projecile-switch-project'.

Each action is made of:

- a key (one-letter string, avoiding \"o\" which is reserved for
  the default action) to call the action, a function of one
- variable (the selected candidate) to execute the action, a
- name (string) for the action.

If you modify this variable outside the Customize interface and
after loading counsel-projectile, then you should evaluate
 
    (ivy-set-actions 
     'counsel-projectile-switch-project
     counsel-projectile-switch-project-actions)

afterwards to apply your changes."
  :type '(repeat
          (list :tag "Action"
                (string   :tag "     key")
                (function :tag "function")
                (string   :tag "    name")))
  :set (lambda (sym val)
         (set sym val)
         (ivy-set-actions 'counsel-projectile-switch-project val))
  :group 'counsel-projectile)

(defcustom counsel-projectile-actions
  '(("j" counsel-projectile-action-other-window
    "other window")
    ("x" counsel-projectile-action-file-extern
     "open file externally")
    ("r" counsel-projectile-action-file-root
     "open file as root"))
  "List of actions for `counsel-projecile'.

Each action is made of:

- a key (one-letter string, avoiding \"o\" which is reserved for
  the default action) to call the action, a function of one
- variable (the selected candidate) to execute the action, a
- name (string) for the action.

If you modify this variable outside the Customize interface and
after loading counsel-projectile, then you should evaluate
 
    (ivy-set-actions 
     'counsel-projectile
     counsel-projectile-actions)

afterwards to apply your changes."
  :type '(repeat
          (list :tag "Action"
                (string   :tag "     key")
                (function :tag "function")
                (string   :tag "    name")))
  :set (lambda (sym val)
         (set sym val)
         (ivy-set-actions 'counsel-projectile val))
  :group 'counsel-projectile)

;;; counsel-projectile-map

(defun counsel-projectile-drop-to-switch-project ()
  "For use in minibuffer maps.  Quit and call
`counsel-projectile-switch-project'."
  (interactive)
  (ivy-quit-and-run
   (counsel-projectile-switch-project)))

(defvar counsel-projectile-drop-to-switch-project-binding "M-SPC"
  "Key binding for `counsel-projectile-drop-to-switch-project' in
  `counsel-projectile-map'.")

(defvar counsel-projectile-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (kbd counsel-projectile-drop-to-switch-project-binding)
      'counsel-projectile-drop-to-switch-project)
    map)
  "Keymap used in the minibuffer.")
  
;;; counsel-projectile-find-file

(defun counsel-projectile-find-file-action (file)
  "Find FILE and run `projectile-find-file-hook'."
  (find-file (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-other-window (file)
  "Find FILE in another window and run
`projectile-find-file-hook'."
  (find-file-other-window (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-extern (file)
  "Find FILE externally and run `projectile-find-file-hook'."
  (counsel-find-file-extern (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-root (file)
  "Find FILE as root and run `projectile-find-file-hook'."
  (counsel-find-file-as-root (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-transformer (name)
  "Transform non-visited file names with `ivy-virtual' face."
  (if (not (get-file-buffer (expand-file-name name (projectile-project-root))))
      (propertize name 'face 'ivy-virtual)
    name))

;;;###autoload
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

Replacement for `projectile-find-file'.  With a prefix ARG
invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (projectile-current-project-files)
            :matcher #'counsel--find-file-matcher
            :require-match t
            :keymap counsel-projectile-map
            :action #'counsel-projectile-find-file-action
            :caller 'counsel-projectile-find-file))

(ivy-set-display-transformer
 'counsel-projectile-find-file
 'counsel-projectile-find-file-transformer)

;;; counsel-projectile-find-dir

(defun counsel-projectile--dir-list ()
  "Return a list of files for the current project."
  (if projectile-find-dir-includes-top-level
      (append '("./") (projectile-current-project-dirs))
    (projectile-current-project-dirs)))

(defun counsel-projectile-find-dir-action (dir)
  "Visit DIR with dired and run `projectile-find-dir-hook'."
  (dired (projectile-expand-root dir))
  (run-hooks 'projectile-find-dir-hook))

(defun counsel-projectile-find-dir-action-other-window (dir)
  "Visit DIR with dired in another window and run
`projectile-find-dir-hook'."
 (dired-other-window (projectile-expand-root dir))
 (run-hooks 'projectile-find-dir-hook))

;;;###autoload
(defun counsel-projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find dir: ")
            (counsel-projectile--dir-list)
            :require-match t
            :keymap counsel-projectile-map
            :action #'counsel-projectile-find-dir-action
            :caller 'counsel-projectile-find-dir))

;;; counsel-projectile-switch-to-buffer

(defun counsel-projectile--buffer-list ()
  "Get a list of project buffer names.

Like `projectile-project-buffer-names', but propertize buffer
names as in `ivy--buffer-list'."
  (let ((buffer-names (projectile-project-buffer-names)))
    (when counsel-projectile-remove-current-buffer
      (setq buffer-names (delete (buffer-name (current-buffer)) buffer-names)))
    (ivy--buffer-list "" nil
                      (lambda (x)
                        (member (car x) buffer-names)))))

(defun counsel-projectile-switch-to-buffer-action (buffer)
  "Switch to BUFFER."
  (switch-to-buffer buffer nil 'force-same-window))

;;;###autoload
(defun counsel-projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to buffer: ")
            (counsel-projectile--buffer-list)
            :matcher #'ivy--switch-buffer-matcher
            :require-match t
            :keymap counsel-projectile-map
            :action #'counsel-projectile-switch-to-buffer-action
            :caller 'counsel-projectile-switch-to-buffer))

(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer
 'ivy-switch-buffer-transformer)

;;; counsel-projectile-grep

(defvar counsel-projectile-grep-base-command "grep -rnE %s -- %%s ."
  "Format string to use in `cousel-projectile-grep-function' to
construct the command.")

(defvar counsel-projectile-grep-command nil)

(defvar counsel-projectile-grep-options-history nil
  "History for `counsel-projectile-grep' options.")

(defun counsel-projectile-grep-function (string)
  "Grep in the current project for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command (format counsel-projectile-grep-command
				      (shell-quote-argument regex)))
      nil)))

(defun counsel-projectile-grep-transformer (str)
  "Higlight file and line number in STR, first removing the
\"./\" prefix from the filename."
  ;; This makes the display consistent with `counsel-git-grep' and
  ;; `counsel-ag'-like commands.
  (counsel-git-grep-transformer (string-remove-prefix "./" str)))

(defun counsel-projectile-grep-occur ()
  "Generate a custom occur buffer for `counsel-projectile-grep'."
  ;; Copied from `counsel-grep-like-occur', except that we don't
  ;; prepend "./" to the candidates since grep already does so.
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory counsel--git-dir))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  (let* ((cmd (format counsel-projectile-grep-command
                      (shell-quote-argument
                       (counsel-unquote-regex-parens
                        (ivy--regex ivy-text)))))
         (cands (split-string (shell-command-to-string cmd) "\n" t)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines cands)))

(defun counsel-projectile-grep (&optional options-or-cmd)
  "Run a grep search in the project.

In a git project, use `counsel-git-grep'.  In a non-git project,
use grep recursively.

OPTIONS-OR-CMD, if non-nil, is a string containing either
additional options to be passed to grep, or an alternative git
grep command. It is read from the minibuffer if the function is
called with a prefix argument."
  (interactive)
  (if (and (eq (projectile-project-vcs) 'git)
           projectile-use-git-grep)
      (let ((counsel-prompt-function
             (lambda ()
               (ivy-add-prompt-count
                (format "%s: " (projectile-prepend-project-name (ivy-state-prompt ivy-last)))))))
        (counsel-git-grep (or current-prefix-arg options-or-cmd)
                          counsel-projectile-grep-initial-input))
    (counsel-require-program (car (split-string counsel-projectile-grep-base-command)))	
    (let* ((ignored-files (mapconcat (lambda (i)
                                       (concat "--exclude="
                                               (shell-quote-argument i)
                                               " "))
                                     (projectile-ignored-files-rel)
                                     ""))
           (ignored-dirs (mapconcat (lambda (i)
                                      (concat "--exclude-dir="
                                              (shell-quote-argument i)
                                              " "))
                                    (projectile-ignored-directories-rel)
                                    ""))
           (ignored (concat ignored-files ignored-dirs))
           (options
            (if current-prefix-arg
                (read-string (projectile-prepend-project-name "grep options: ")
                             ignored
                             'counsel-projectile-grep-options-history)
              (concat ignored options-or-cmd))))
      (setq counsel-projectile-grep-command
            (format counsel-projectile-grep-base-command options))
      (ivy-set-prompt 'counsel-projectile-grep counsel-prompt-function)
      (setq counsel--git-dir (projectile-project-root))
      (ivy-read (projectile-prepend-project-name "grep")
                #'counsel-projectile-grep-function
                :initial-input counsel-projectile-grep-initial-input
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))
                :caller 'counsel-projectile-grep))))

(counsel-set-async-exit-code 'counsel-projectile-grep 1 "No matches found")
(ivy-set-occur 'counsel-projectile-grep 'counsel-projectile-grep-occur)
(ivy-set-display-transformer 'counsel-projectile-grep  'counsel-projectile-grep-transformer)

;;; counsel-projectile-ag

(defvar counsel-projectile-ag-options-history nil
  "History for `counsel-projectile-ag' options.")

;;;###autoload
(defun counsel-projectile-ag (&optional options)
  "Run an ag search in the project.

OPTIONS, if non-nil, is a string containing additional options to
be passed to ag. It is read from the minibuffer if the function
is called with a prefix argument."
  (interactive)
  (let* ((ignored (mapconcat (lambda (i)
                               (concat "--ignore "
                                       (shell-quote-argument i)
                                       " "))
                             (append (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             ""))
         (options
          (if current-prefix-arg
              (read-string (projectile-prepend-project-name "ag options: ")
                           ignored
                           'counsel-projectile-ag-options-history)
            (concat ignored options))))
    (counsel-ag (eval counsel-projectile-ag-initial-input)
                (projectile-project-root)
                options
                (projectile-prepend-project-name "ag"))))

;;; counsel-projectile-rg

(defvar counsel-projectile-rg-options-history nil
  "History for `counsel-projectile-rg' options.")

;;;###autoload
(defun counsel-projectile-rg (&optional options)
  "Run an rg search in the project.

OPTIONS, if non-nil, is a string containing additional options to
be passed to rg. It is read from the minibuffer if the function
is called with a prefix argument."
  (interactive)
  (let* ((ignored (mapconcat (lambda (i)
                               (concat "--glob "
                                       (shell-quote-argument (concat "!" i))
                                       " "))
                             (append (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             ""))
         (options
          (if current-prefix-arg
              (read-string (projectile-prepend-project-name "rg options: ")
                           ignored
                           'counsel-projectile-rg-options-history)
            (concat ignored options))))
    (counsel-rg (eval counsel-projectile-rg-initial-input)
                (projectile-project-root)
                options
                (projectile-prepend-project-name "rg"))))

;;; counsel-projectile-org-capture

;;;###autoload
(defun counsel-projectile-org-capture ()
  "Capture something into current project."
  (interactive)
  (require 'org-capture)
  (let* ((root (projectile-project-root))
	 (name (projectile-project-name))
	 (org-capture-templates
	  (cl-loop
	   for template in counsel-projectile-org-capture-templates
	   collect (cl-loop
		    for item in template
		    if (= (cl-position item template) 3) ;; template's target
		    collect (cl-loop
			     for x in item
			     if (stringp x)
			     collect (replace-regexp-in-string
				      "\\${[^}]+}"
				      (lambda (s)
					(pcase s
					  ("${root}" root)
					  ("${name}" name)))
				      x)
			     else
			     collect x)
		    else
		    collect item)))
	 (org-capture-templates-contexts counsel-projectile-org-capture-templates-contexts)
         (ivy--prompts-list ivy--prompts-list))
    (ivy-set-prompt 'counsel-org-capture
                    (lambda ()
                      (ivy-add-prompt-count
                       (projectile-prepend-project-name (ivy-state-prompt ivy-last)))))
    (counsel-org-capture)))

;;; counsel-projectile-switch-project

(defun counsel-projectile-switch-project-action (project)
  "Switch to PROJECT.
Invokes the command referenced by
`projectile-switch-project-action' on switch.

This is a replacement for `projectile-switch-project-by-name'
with a different switching mechanism: the switch-project action
is called from a dedicated buffer rather than the initial buffer.
Also, PROJECT's dir-local variables are loaded before calling the
action."
  (run-hooks 'projectile-before-switch-project-hook)
  ;; Kill and recreate the switch buffer to get rid of any local
  ;; variable
  (ignore-errors (kill-buffer " *counsel-projectile*"))
  (set-buffer (get-buffer-create " *counsel-projectile*"))
  (setq default-directory project)
  ;; Load the project dir-local variables into the switch buffer, so
  ;; the action can make use of them
  (hack-dir-local-variables-non-file-buffer)
  (funcall projectile-switch-project-action)
  ;; If the action relies on `ivy-read' then, after one of its
  ;; `ivy-read' actions is executed, the current buffer will be set
  ;; back to the initial buffer. Hence we make sure tu evaluate
  ;; `projectile-after-switch-project-hook' from the switch buffer.
  (with-current-buffer " *counsel-projectile*"
    (run-hooks 'projectile-after-switch-project-hook)))

(defun counsel-projectile-switch-project-action-find-file (project)
  "Action for `counsel-projectile-switch-project' to find a
PROJECT file."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-projectile-find-file ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-find-file-manually (project)
  "Action for `counsel-projectile-switch-project' to find a
PROJECT file manually."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-find-file project))))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-find-dir (project)
  "Action for `counsel-projectile-switch-project' to find a
PROJECT directory."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-projectile-find-dir ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-switch-to-buffer (project)
  "Action for `counsel-projectile-switch-project' to switch to a
PROJECT buffer."
  (let ((projectile-switch-project-action 'counsel-projectile-switch-to-buffer))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-save-all-buffers (project)
  "Action for `counsel-projectile-switch-project' to save all
PROJECT buffers."
  (let ((projectile-switch-project-action 'projectile-save-project-buffers))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-kill-buffers (project)
  "Action for `counsel-projectile-switch-project' to kill all
PROJECT buffers."
  (let ((projectile-switch-project-action 'projectile-kill-buffers))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-remove-known-project (project)
  "Action for `counsel-projectile-switch-project' to remove
PROJECT from the list of known projects."
  (projectile-remove-known-project project)
  (setq ivy--all-candidates
        (delete project ivy--all-candidates))
  (ivy--reset-state ivy-last))

(defun counsel-projectile-switch-project-action-edit-dir-locals (project)
  "Action for `counsel-projectile-switch-project' to edit
PROJECT's dir-locals."
  (let ((projectile-switch-project-action 'projectile-edit-dir-locals))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-vc (project)
  "Action for `counsel-projectile-switch-project' to open PROJECT
in vc-dir / magit / monky."
  (let ((projectile-switch-project-action 'projectile-vc))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-run-eshell (project)
  "Action for `counsel-projectile-switch-project' to start
`eshell' from PROJECT's root."
  (let ((projectile-switch-project-action 'projectile-run-eshell))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-grep (project)
  "Action for `counsel-projectile-switch-project' to search
PROJECT with `grep'."
  (let ((projectile-switch-project-action 'counsel-projectile-ag))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-ag (project)
  "Action for `counsel-projectile-switch-project' to search
PROJECT with `ag'."
  (let ((projectile-switch-project-action 'counsel-projectile-ag))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-rg (project)
  "Action for `counsel-projectile-switch-project' to search
PROJECT with `rg'."
  (let ((projectile-switch-project-action 'counsel-projectile-rg))
    (counsel-projectile-switch-project-action project)))

(defun counsel-projectile-switch-project-action-org-capture (project)
  "Action for `counsel-projectile-switch-project' to capture
something into PROJECT."
  (let ((projectile-switch-project-action 'counsel-projectile-org-capture))
    (counsel-projectile-switch-project-action project)))

;;;###autoload
(defun counsel-projectile-switch-project ()
  "Switch to a project we have visited before."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            (if counsel-projectile-remove-current-project
                (projectile-relevant-known-projects)
              projectile-known-projects)
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action #'counsel-projectile-switch-project-action
            :require-match t
            :caller 'counsel-projectile-switch-project))

;;; counsel-projectile

(defvar counsel-projectile--buffers nil
  "Stores the list of project buffers.")

(defvar counsel-projectile--non-visited-files nil
  "Stores the list of project files that are not currently visited by a buffer.")

(defun counsel-projectile--buffer-file-list ()
  "Get a list of project buffers and files."
  (append
   (setq counsel-projectile--buffers
         (counsel-projectile--buffer-list))
   (setq counsel-projectile--non-visited-files
         (let ((root (projectile-project-root))
               (files (projectile-current-project-files))
               file)
           (dolist (buffer counsel-projectile--buffers files)
             (when (setq file (buffer-file-name (get-buffer buffer)))
               (setq files (delete (file-relative-name file root) files))))))))

(defun counsel-projectile--matcher (regexp candidates)
  "Return REGEXP-matching CANDIDATES.

Relies on `ivy--switch-buffer-matcher' and
`counsel--find-file-matcher'."
  (append (ivy--switch-buffer-matcher regexp counsel-projectile--buffers)
          (counsel--find-file-matcher regexp counsel-projectile--non-visited-files)))

(defun counsel-projectile-action (name)
  "Switch to buffer or find file named NAME."
  (if (member name counsel-projectile--buffers)
      (counsel-projectile-switch-to-buffer-action name)
    (counsel-projectile-find-file-action name)))

(defun counsel-projectile-action-other-window (name)
  "Switch to buffer or find file named NAME in another window."
  (if (member name counsel-projectile--buffers)
      (switch-to-buffer-other-window name)
    (counsel-projectile-find-file-action-other-window name)))

(defun counsel-projectile-action-file-extern (name)
  "Find file named NAME externally."
  (if (member name counsel-projectile--buffers)
      (message "This action does not apply to buffers.")
    (counsel-projectile-find-file-action-extern name)))

(defun counsel-projectile-action-file-root (name)
  "Find file named NAME as root."
  (if (member name counsel-projectile--buffers)
      (message "This action does not apply to buffers.")
    (counsel-projectile-find-file-action-root name)))

(defun counsel-projectile-transformer (name)
  "Fontifies modified, file-visiting buffers as well as non-visited files."
  (if (member name counsel-projectile--buffers)
      (ivy-switch-buffer-transformer name)
    (propertize name 'face 'ivy-virtual)))

;;;###autoload
(defun counsel-projectile (&optional arg)
  "Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (if (not (projectile-project-p))
      (counsel-projectile-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (ivy-read (projectile-prepend-project-name "Load buffer or file: ")
              (counsel-projectile--buffer-file-list)
              :matcher #'counsel-projectile--matcher
              :require-match t
              :keymap counsel-projectile-map
              :action #'counsel-projectile-action
              :caller 'counsel-projectile)))

(ivy-set-display-transformer
 'counsel-projectile
 'counsel-projectile-transformer)

;;; counsel-projectile-mode

(defvar counsel-projectile-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map projectile-command-map)
    (define-key map (kbd "s r") 'counsel-projectile-rg)
    (define-key map (kbd "C-c") 'counsel-projectile-org-capture)
    (define-key map (kbd "SPC") 'counsel-projectile)
    map)
  "Keymap for Counesl-Projectile commands after `projectile-keymap-prefix'.")
(fset 'counsel-projectile-command-map counsel-projectile-command-map)

(defvar counsel-projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-keymap-prefix 'counsel-projectile-command-map)
    (define-key map [remap projectile-find-file] 'counsel-projectile-find-file)
    (define-key map [remap projectile-find-dir] 'counsel-projectile-find-dir)
    (define-key map [remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer)
    (define-key map [remap projectile-grep] 'counsel-projectile-grep)
    (define-key map [remap projectile-ag] 'counsel-projectile-ag)
    (define-key map [remap projectile-switch-project] 'counsel-projectile-switch-project)
    map)
  "Keymap for Counsel-Projectile mode.")

;;;###autoload
(define-minor-mode counsel-projectile-mode
  "Toggle Counsel-Projectile mode on or off.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Counsel-Projectile mode triggers Projectile mode, remaps
Projectile commands that have counsel replacements, and adds key
bindings for Counsel-Projectile commands that have no Projectile
counterpart.

\\{counsel-projectile-mode-map}"
  :group 'counsel-projectile
  :require 'counsel-projectile
  :keymap counsel-projectile-mode-map
  :global t
  (if counsel-projectile-mode
      (projectile-mode)
    (projectile-mode -1)))

(provide 'counsel-projectile)

;;; counsel-projectile.el ends here
