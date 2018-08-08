;;; counsel-projectile.el --- Ivy integration for Projectile -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Eric Danan

;; Author: Eric Danan
;; URL: https://github.com/ericdanan/counsel-projectile
;; Keywords: project, convenience
;; Version: 0.2.0
;; Package-Requires: ((counsel "0.10.0") (projectile "0.14.0"))

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
;; system. Counsel-projectile provides further ivy integration into
;; projectile by taking advantage of ivy's support for selecting from
;; a list of actions and applying an action without leaving the
;; completion session. Concretely, counsel-projectile defines
;; replacements for existing projectile commands as well as new
;; commands that have no projectile counterparts. A minor mode is also
;; provided that adds key bindings for all these commands on top of
;; the projectile key bindings.
;;
;;; Code:
;;;; require

(require 'counsel)
(require 'projectile)

;;;; global

(defgroup counsel-projectile nil
  "Ivy integration for Projectile."
  :group 'ivy
  :group 'projectile)

(defun counsel-projectile--defcustom-action (command action group)
  "Create a custom variable named \"COMMAND-action\" in GROUP,
with default value ACTION, to be used as `:action' parameter for
COMMAND's `ivy-read' call.

This variable holds either a single action function, or an action
list whose first element is the index of the default action in
the list and the remaining elements are the actions (a key, a
function, and a name for each action)."
  (eval
   `(defcustom ,(intern (format "%s-action" command))
      ',action
      ,(format "Action(s) for `%s'.

This variable holds either a single action function (function of
one variable, the selected candidate) or an action list
consisting of:

- the index of the default action in the list (1 for the first
  action, etc),
- the available actions, each of which consists of:
  - a key (string) to call the action,
  - an action function of one variable,
  - a name (string) for the action.

In both cases, extra actions can be added with `ivy-set-actions'.
An action is triggered for the selected candidate with `M-o
<key>' or `C-M-o <key>'.  The default action can also be
triggered with `M-RET' or `C-M-RET'. If this variable holds a
single action function, this action becomes the default action
and is assigned the key \"o\".  For an action list, it is also
usual to assign the key \"o\" to the default action." command)
      :type '(choice
              (function :tag "Single action function")
              (cons :tag "Action list"
                    (integer :tag "Index of default action" :value 1)
                    (repeat :tag "Actions"
                            (list :tag "Action"
                                  (string   :tag "     key")
                                  (function :tag "function")
                                  (string   :tag "    name")))))
      :group ',group)))

(defun counsel-projectile--action-index (action-item action-list)
  "Return the index in ACTION-LIST of the action whose key,
function, name, or index in the list (1 for the first action,
etc) is ACTION-ITEM.  If there is no such action, throw an error.

ACTION-LIST is an action list whose first element is the index of
the default action in the list and the remaining elements are the
actions (a key, a function, and a name for each action)."
  (let (index)
    (if (integerp action-item)
        (when (< 0 action-item (length action-list))
          (setq index action-item))
      (setq index (cl-position-if
                   (cond
                    ((functionp action-item)
                     (lambda (action)
                       (equal action-item
                              (cadr action))))
                    ((stringp action-item)
                     (lambda (action)
                       (member action-item
                               (list (car action) (cl-caddr action))))))
                   (cdr action-list)))
      (when index
        (setq index (1+ index))))
    (or index
        (error "Action not found: %s" action-item))))

(defun counsel-projectile-modify-action (action-var modifications)
  "Make MODIFICATIONS to ACTION-VAR.

ACTION-VAR is a variable holding an action list whose first
element is the index of the default action in the list and the
remaining elements are the actions (a key, a function, and a name
for each action).

MODIFICATIONS is a list of modifications to be applied
sequentially to ACTION-LIST. Each modification has one of the
following formats:

    (remove ACTION-ITEM)
        Remove the action whose key, function, name, or index in
        the list (1 for the first action, etc) is ACTION-ITEM
        from the list.

    (add ACTION TARGET-ITEM)
        Add ACTION (a list containing a key, a function, and a
        name) to the list, just before the action whose key,
        function, name, or index in the list (1 for the first
        action, etc) is TARGET-ITEM.  If TARGET-ITEM is omitted,
        add the action at the end of the list.

    (move ACTION-ITEM TARGET-ITEM)
        Move the action whose key, function, name, or index in
        the list (1 for the first action, etc) is ACTION-ITEM
        just before the action whose key, function, name, or
        index in the list (1 for the first action, etc) is
        TARGET-ITEM.  If TARGET-ITEM is omitted, move the action
        to the end of the list.

    (setkey ACTION-ITEM KEY)
        Set the key of the action whose key, function, name, or
        index in the list (1 for the first action, etc) is
        ACTION-ITEM to KEY.

    (setfun ACTION-ITEM FUNCTION)
        Set the function of the action whose key, function, name,
        or index in the list (1 for the first action, etc) is
        ACTION-ITEM to FUNCTION.

    (setname ACTION-ITEM NAME)
        Set the name of the action whose key, function, name, or
        index in the list (1 for the first action, etc) is
        ACTION-ITEM to NAME.

    (default ACTION-ITEM)
        Set the index of the default action in the list to that
        of the action whose key, function, name, or index in the
        list (1 for the first action, etc) is ACTION-ITEM.

If anything goes wrong, throw an error and do not modify ACTION-VAR."
  (let ((action-list (symbol-value action-var))
        mod)
    ;; Make sure ACTION-VAR actually holds a list and not a single
    ;; action function
    (unless (listp action-list)
      (error "%s's value is not a list" action-var))
    (while (setq mod (pop modifications))
      (pcase mod
        (`(remove ,action-item)
         (setq action-list
               (remove (nth (counsel-projectile--action-index action-item action-list)
                            action-list)
                       action-list)))
        (`(add ,action ,target-item)
         (let ((index (counsel-projectile--action-index target-item action-list)))
           ;; copied from `helm-append-at-nth'
           (setq action-list (cl-loop for a in action-list
                                      for count from 1
                                      collect a
                                      when (= count index)
                                      collect action))))
        (`(add ,action)
         (setq action-list (append action-list (list action))))
        (`(move ,action-item ,target-item)
         (push `(add ,(nth (counsel-projectile--action-index action-item action-list)
                           action-list)
                     ,target-item)
               modifications)
         (push `(remove ,action-item)
               modifications))
        (`(move ,action-item)
         (push `(add ,(nth (counsel-projectile--action-index action-item action-list)
                           action-list))
               modifications)
         (push `(remove ,action-item)
               modifications))
        (`(setkey ,action-item ,key)
         (let ((index (counsel-projectile--action-index action-item action-list)))
           (setq action-list (cl-loop for a in action-list
                                      for count from 0
                                      if (= count index)
                                      collect (cons key (cdr a))
                                      else
                                      collect a))))
        (`(setfun ,action-item ,fun)
         (let ((index (counsel-projectile--action-index action-item action-list)))
           (setq action-list (cl-loop for a in action-list
                                      for count from 0
                                      if (= count index)
                                      collect (list (car a) fun (cl-caddr a))
                                      else
                                      collect a))))
        (`(setname ,action-item ,name)
         (let ((index (counsel-projectile--action-index action-item action-list)))
           (setq action-list (cl-loop for a in action-list
                                      for count from 0
                                      if (= count index)
                                      collect (list (car a) (cadr a) name)
                                      else
                                      collect a))))
        (`(default ,action-item)
         (setq action-list
               (cons (counsel-projectile--action-index action-item action-list)
                     (cdr action-list))))))
    (set action-var action-list)))

;;;; counsel-projectile-find-file

(defcustom counsel-projectile-sort-files nil
  "Non-nil if files should be sorted in
`counsel-projectile-find-file' and `counsel-projectile'.

The sorting function can be modified by adding an entry for
`counsel-projectile-find-file' in `ivy-sort-functions-alist'."
  :type 'boolean
  :group 'counsel-projectile)

(defcustom counsel-projectile-find-file-matcher 'counsel--find-file-matcher
  "Function returning candidates matching minibuffer input in
`counsel-projectile-find-file', also used to match files in
`counsel-projectile'.

Several choices are proposed:

- Ivy generic matcher (`ivy--re-filter'). This is the matcher
  used by default in all ivy commands.

- Counsel matcher (`counsel--find-file-matcher').  This is the
  matcher used in `counsel-find-file', allowing to ignore some
  files based on `counsel-find-file-ignore-regexp'.

- Counsel-projectile basename
  matcher (`counsel-projectile-basename-matcher').  This one only
  displays files whose basename matches minibuffer input, or if
  there is none all files whose name (relative to the project
  root) matches. It also uses the counsel matcher to ignore some
  files.

It is also possible to use a custom matcher.  It must be a function taking two argument, the regexp and the candidates (see e.g. `counsel--find-file-matcher')."
  :type '(choice
          (const :tag "Ivy generic matcher" ivy--re-filter)
          (const :tag "Counsel matcher" counsel--find-file-matcher)
          (const :tag "Counsel-projectile basename matcher" counsel-projectile-find-file-matcher-basename)
          (function :tag "Custom function"))
  :group 'counsel-projectile)

(counsel-projectile--defcustom-action
 'counsel-projectile-find-file
 '(1
   ("o" counsel-projectile-find-file-action
    "current window")
   ("j" counsel-projectile-find-file-action-other-window
    "other window")
   ("x" counsel-projectile-find-file-action-extern
    "open externally")
   ("r" counsel-projectile-find-file-action-root
    "open as root")
   ("m" counsel-projectile-find-file-action-find-file-manually
    "find file manually")
   ("p" (lambda (_) (counsel-projectile-switch-project))
    "switch project"))
 'counsel-projectile)

(defun counsel-projectile-find-file-matcher-basename (regexp candidates)
  "Return the list of CANDIDATES whose basename matches REGEXP,
or if there is none the list of all CANDIDATES matching REGEXP.
Also uses `counsel--find-file-matcher' to ignore candidates based
on `counsel-find-file-ignore-regexp'."
  (let ((cands (ivy--re-filter regexp candidates)))
    (or (and (not (string= ivy-text ""))
             ;; We first filter `cands' to retain only matches in file
             ;; basename. This is almost copied from `ivy--re-filter'
             ;; because we can't quite use it directly.
             (let ((re-list (if (stringp regexp)
                                (list (cons regexp t))
                              regexp))
                   (res cands))
               (dolist (re re-list)
                 (setq res
                       (ignore-errors
                         (funcall
                          (if (cdr re)
                              #'cl-remove-if-not
                            #'cl-remove-if)
                          (let ((re-str (car re)))
                            (lambda (x)
                              (string-match re-str
                                            (file-name-nondirectory x))))
                          res))))
               ;; We then apply `counsel--find-file-matcher' to `res'
               ;; so we can honor `ivy-use-ignore', but we don't need
               ;; to filter again.
               (counsel--find-file-matcher nil res)))
        ;; We apply `counsel--find-file-matcher' to `cands' so we can
        ;; honor `ivy-use-ignore', but we don't need to filter
        ;; again.
        (counsel--find-file-matcher nil cands))))     

(defun counsel-projectile-find-file-action (file)
  "Find FILE and run `projectile-find-file-hook'."
  (find-file (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-other-window (file)
  "Find FILE in another window and run
`projectile-find-file-hook'."
  (find-file-other-window (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-find-file-manually (file)
  "Call `counsel-find-file' from FILE's directory."
  (let* ((f (projectile-expand-root file))
         (default-directory (file-name-directory f)))
    (counsel-find-file)))

(defun counsel-projectile-find-file-action-extern (file)
  "Find FILE externally and run `projectile-find-file-hook'."
  (counsel-find-file-extern (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-action-root (file)
  "Find FILE as root and run `projectile-find-file-hook'."
  (counsel-find-file-as-root (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile-find-file-transformer (str)
  "Transform non-visited file names with `ivy-virtual' face."
  (if (not (get-file-buffer (projectile-expand-root str)))
      (propertize str 'face 'ivy-virtual)
    str))

;;;###autoload
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a file in the current project.

With a prefix ARG, invalidate the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (projectile-current-project-files)
            :matcher counsel-projectile-find-file-matcher
            :require-match t
            :sort counsel-projectile-sort-files
            :action counsel-projectile-find-file-action
            :caller 'counsel-projectile-find-file))

(ivy-set-display-transformer
 'counsel-projectile-find-file
 'counsel-projectile-find-file-transformer)

;;;; counsel-projectile-find-dir

(defcustom counsel-projectile-sort-directories nil
  "Non-nil if directories should be sorted in
`counsel-projectile-find-dir'.

The sorting function can be modified by adding an entry for
`counsel-projectile-find-dir' in `ivy-sort-functions-alist'."
  :type 'boolean
  :group 'counsel-projectile)

(counsel-projectile--defcustom-action
 'counsel-projectile-find-dir
 '(1
   ("o" counsel-projectile-find-dir-action
    "current window")
   ("j" counsel-projectile-find-dir-action-other-window
    "other window")
   ("x" counsel-projectile-find-dir-action-extern
    "open externally")
   ("r" counsel-projectile-find-dir-action-root
    "open as root")
   ("m" counsel-projectile-find-file-action-find-file-manually
    "find file manually")
   ("p" (lambda (_) (counsel-projectile-switch-project))
    "switch project"))
 'counsel-projectile)
 
(defun counsel-projectile--project-directories ()
  "Return a list of current project's directories."
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

(defun counsel-projectile-find-dir-action-extern (dir)
  "Visit DIR externally and run `projectile-find-dir-hook'."
  (counsel-find-file-extern (projectile-expand-root dir))
  (run-hooks 'projectile-find-dir-hook))

(defun counsel-projectile-find-dir-action-root (dir)
  "Visit DIR as root and run `projectile-find-dir-hook'."
  (counsel-find-file-as-root (projectile-expand-root dir))
  (run-hooks 'projectile-find-dir-hook))

(defun counsel-projectile-find-dir-transformer (str)
  "Transform candidates with `ivy-subdir' face."
  (propertize str 'face 'ivy-subdir))

;;;###autoload
(defun counsel-projectile-find-dir (&optional arg)
  "Jump to a directory in the current project.

With a prefix ARG, invalidate the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find dir: ")
            (counsel-projectile--project-directories)
            :require-match t
            :sort counsel-projectile-sort-directories
            :action counsel-projectile-find-dir-action
            :caller 'counsel-projectile-find-dir))

(ivy-set-display-transformer
 'counsel-projectile-find-dir
 'counsel-projectile-find-dir-transformer)

;;;; counsel-projectile-switch-to-buffer

(defcustom counsel-projectile-sort-buffers nil
  "Non-nil if buffers should be sorted in
`counsel-projectile-switch-to-buffer' and `counsel-projectile'.

The sorting function can be modified by adding an entry for
`counsel-projectile-switch-to-buffer' in
`ivy-sort-functions-alist'."
  :type 'boolean
  :group 'counsel-projectile)

(defcustom counsel-projectile-remove-current-buffer nil
  "Non-nil if current buffer should be removed from the
candidates list of `counsel-projectile-switch-to-buffer' and
`counsel-projectile'."
  :type 'boolean
  :group 'counsel-projectile)

(counsel-projectile--defcustom-action
 'counsel-projectile-switch-to-buffer
 '(1
   ("o" counsel-projectile-switch-to-buffer-action
    "current window")
   ("j" switch-to-buffer-other-window
    "other window")
   ("k" ivy--kill-buffer-action
    "kill")
   ("m" counsel-projectile-switch-to-buffer-action-find-file-manually
    "find file manually")
   ("p" (lambda (_) (counsel-projectile-switch-project))
    "switch project"))
 'counsel-projectile)

(defvar counsel-projectile-switch-to-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") (lambda ()
                                      (interactive)
                                      (ivy--kill-buffer-action (ivy-state-current ivy-last))))
    map)
  "Keymap for `counsel-projectile-switch-to-buffer'.")

(defun counsel-projectile--project-buffers (&rest _)
  ;; The ignored arguments are so that the function can be used as
  ;; collection function in `counsel-projectile-switch-to-buffer'.
  "Return a list of buffers in the current project.

Like `projectile-project-buffer-names', but propertize buffer
names as in `ivy--buffer-list', and remove current buffer if
`counsel-projectile-remove-currennt-buffer' is non-nil."
  (let ((buffer-names (projectile-project-buffer-names)))
    (when counsel-projectile-remove-current-buffer
      (setq buffer-names (delete (buffer-name (current-buffer)) buffer-names)))
    (ivy--buffer-list "" nil
                      (lambda (x)
                        (member (car x) buffer-names)))))

(defun counsel-projectile-switch-to-buffer-action (buffer)
  "Switch to BUFFER."
  (switch-to-buffer buffer nil 'force-same-window))

(defun counsel-projectile-switch-to-buffer-action-find-file-manually (buffer)
  "Call `counsel-find-file' from BUFFER's default directory."
  (let* ((b (get-buffer buffer))
         (default-directory
           (or (and b (buffer-local-value 'default-directory b))
               (projectile-project-root))))
    (counsel-find-file)))

(defun counsel-projectile-switch-to-buffer-transformer (str)
  "Transform candidate STR when switching project buffers.

This simply applies the same transformer as in `ivy-switch-buffer', which is `ivy-switch-buffer-transformer' by default but could have been modified e.g. by the ivy-rich package."
  (funcall (plist-get ivy--display-transformers-list 'ivy-switch-buffer)
           str))

;;;###autoload
(defun counsel-projectile-switch-to-buffer ()
  "Jump to a buffer in the current project."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to buffer: ")
            ;; We use a collection function so that it is called each
            ;; time the `ivy-state' is reset. This is needed for the
            ;; "kill buffer" action.
            #'counsel-projectile--project-buffers
            :matcher #'ivy--switch-buffer-matcher
            :require-match t
            :sort counsel-projectile-sort-buffers
            :action counsel-projectile-switch-to-buffer-action
            :keymap counsel-projectile-switch-to-buffer-map
            :caller 'counsel-projectile-switch-to-buffer))

(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer
 'counsel-projectile-switch-to-buffer-transformer)

;;;; counsel-projectile-grep

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

(defvar counsel-projectile-grep-base-command "grep -rnE %s -- %%s ."
  "Format string to use in `cousel-projectile-grep-function' to
construct the command.")

(defvar counsel-projectile-grep-command nil)

(defvar counsel-projectile-grep-options-history nil
  "History for `counsel-projectile-grep' options.")

(defun counsel-projectile-grep-function (string)
  "Grep for STRING in the current project."
  (or (counsel-more-chars)
      (let ((default-directory (ivy-state-directory ivy-last))
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
    (setq default-directory (ivy-state-directory ivy-last)))
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
  "Search the current project with grep.

If inside a git project and `projectile-use-git-grep' is non-nil,
use `counsel-git-grep'. Otherwise use grep recursively.

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
              (concat ignored options-or-cmd)))
           (default-directory (projectile-project-root)))
      (setq counsel-projectile-grep-command
            (format counsel-projectile-grep-base-command options))
      (ivy-set-prompt 'counsel-projectile-grep counsel-prompt-function)
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

;;;; counsel-projectile-ag

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

(defvar counsel-projectile-ag-options-history nil
  "History for `counsel-projectile-ag' options.")

;;;###autoload
(defun counsel-projectile-ag (&optional options)
  "Search the current project with ag.

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

;;;; counsel-projectile-rg

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

(defvar counsel-projectile-rg-options-history nil
  "History for `counsel-projectile-rg' options.")

;;;###autoload
(defun counsel-projectile-rg (&optional options)
  "Search the current project with rg.

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

;;;; counsel-projectile-org-capture

(defvar org-capture-templates)
(defvar org-capture-templates-contexts)

(defcustom counsel-projectile-org-capture-templates
  '(("t" "[${name}] Task" entry (file+headline "${root}/notes.org" "Tasks")
     "* TODO %?\n  %u\n  %a"))
  "Project-specific templates for the creation of new entries
with `counsel-projectile-org-capture'.

The format is the same as in `org-capture-templates', except that
in a template's name or target, the placeholders \"${root}\" and
\"${name}\" can be used to stand for the current project root and
name, respectively.

The default value contains a single template, whose name is
\"[${name}] Task\" and whose target is:

    \(file+headline \"${root}/notes.org}\" \"Tasks\"\)

This points to headline \"Tasks\" in file \"notes.org\" in the
project root directory (one file per project).

Two other examples of valid targets are:

    \(file+headline \"${root}/${name}.org}\" \"Tasks\"\)
    \(file+olp \"~/notes.org\" \"${root}\" \"Tasks\"\)

The first one is similar to the default value's target, except
that the file is named after the project name (this can be handy
if you use org-mode's agenda since the project name is then
displayed as category). The second one points to outline path
\"<project-root>/Tasks\" in file \"~/notes.org\" (same file for
all projects)."
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

;;;###autoload
(defun counsel-projectile-org-capture (&optional from-buffer)
  "Capture into the current project.

This command is a replacement for `org-capture' (or
`counsel-org-capture') offering project-specific capture
templates, in addition to the regular templates available from
`org-capture'. These project templates, which are \"expanded\"
relatively to the current project, are determined by the
variables `counsel-projectile-org-capture-templates' and
`counsel-projectile-org-capture-templates-contexts'. See the
former variable in particular for details.

Optional argument FROM-BUFFER specifies the buffer from which to
capture."
  (interactive)
  (require 'org-capture)
  (let* ((root (ignore-errors (projectile-project-root)))
         (name (projectile-project-name))
         (org-capture-templates-contexts
          (append (when root
                    counsel-projectile-org-capture-templates-contexts)
                  org-capture-templates-contexts))
         (org-capture-templates
          (append
           (when root
             (cl-loop
              with replace-fun = `(lambda (string)
                                    (replace-regexp-in-string
                                     "\\${[^}]+}"
                                     (lambda (s)
                                       (pcase s
                                         ("${root}" ,root)
                                         ("${name}" ,name)))
                                     string))
              for template in counsel-projectile-org-capture-templates
              collect (cl-loop
                       for item in template
                       if (= (cl-position item template) 1) ;; template's name
                       collect (funcall replace-fun item)
                       else if (= (cl-position item template) 3) ;; template's target
                       collect (cl-loop
                                for x in item
                                if (stringp x)
                                collect (funcall replace-fun x)
                                else
                                collect x)
                       else
                       collect item)))
           org-capture-templates)))
    (with-current-buffer (or from-buffer (current-buffer))
      (counsel-org-capture))))

;;;; counsel-projectile-org-agenda

;;;###autoload
(defun counsel-projectile-org-agenda (&optional arg org-keys restriction)
  "Open project agenda.

This command simply calls `org-agenda' after filtering out all
agenda files that do not belong to the current project.

Optional arguments ARG, ORG-KEYS, and RESTRICTION are as in
`org-agenda'."
  (interactive "P")
  (require 'org-agenda)
  (let* ((root (projectile-project-root))
         (org-agenda-files
          (cl-remove-if-not (lambda (file)
                              (string-prefix-p root file))
                            (org-agenda-files t 'ifmode))))
    (org-agenda arg org-keys restriction)))

;;;; counsel-projectile-switch-project

(defcustom counsel-projectile-sort-projects nil
  "Non-nil if projects should be sorted in
`counsel-projectile-switch-project'.

The sorting function can be modified by adding an entry for
`counsel-projectile-switch-project' in
`ivy-sort-functions-alist'."
  :type 'boolean
  :group 'counsel-projectile)

(defcustom counsel-projectile-remove-current-project nil
  "Non-nil if current project should be removed from the
candidates list of `counsel-projectile-switch-project'."
  :type 'boolean
  :group 'counsel-projectile)

(counsel-projectile--defcustom-action
 'counsel-projectile-switch-project
 '(1
   ("o" counsel-projectile-switch-project-action
    "jump to a project buffer or file")
   ("f" counsel-projectile-switch-project-action-find-file
    "jump to a project file")
   ("d" counsel-projectile-switch-project-action-find-dir
    "jump to a project directory")
   ("b" counsel-projectile-switch-project-action-switch-to-buffer
    "jump to a project buffer")
   ("m" counsel-projectile-switch-project-action-find-file-manually
    "find file manually from project root")
   ("S" counsel-projectile-switch-project-action-save-all-buffers
    "save all project buffers")
   ("k" counsel-projectile-switch-project-action-kill-buffers
    "kill all project buffers")
   ("K" counsel-projectile-switch-project-action-remove-known-project
    "remove project from known projects")
   ("c" counsel-projectile-switch-project-action-compile
    "run project compilation command")
   ("C" counsel-projectile-switch-project-action-configure
    "run project configure command")
   ("E" counsel-projectile-switch-project-action-edit-dir-locals
    "edit project dir-locals")
   ("v" counsel-projectile-switch-project-action-vc
    "open project in vc-dir / magit / monky")
   ("sg" counsel-projectile-switch-project-action-grep
    "search project with grep")
   ("ss" counsel-projectile-switch-project-action-ag
    "search project with ag")
   ("sr" counsel-projectile-switch-project-action-rg
    "search project with rg")
   ("xs" counsel-projectile-switch-project-action-run-shell
    "invoke shell from project root")
   ("xe" counsel-projectile-switch-project-action-run-eshell
    "invoke eshell from project root")
   ("xt" counsel-projectile-switch-project-action-run-term
    "invoke term from project root")
   ("Oc" counsel-projectile-switch-project-action-org-capture
    "capture into project")
   ("Oa" counsel-projectile-switch-project-action-org-agenda
    "open project agenda"))
 'counsel-projectile)

(defun counsel-projectile-switch-project-by-name (project)
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

(defun counsel-projectile-switch-project-action (project)
  "Jump to a file or buffer in PROJECT."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-projectile ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-find-file (project)
  "Jump to a file in PROJECT."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-projectile-find-file ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-find-file-manually (project)
  "Call `find-file' from PROJECT's root."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-find-file project))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-find-dir (project)
  "Jump to a directory in PROJECT."
  (let ((projectile-switch-project-action
         (lambda ()
           (counsel-projectile-find-dir ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-switch-to-buffer (project)
  "Jump to a buffer in PROJECT."
  (let ((projectile-switch-project-action 'counsel-projectile-switch-to-buffer))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-save-all-buffers (project)
  "Save all buffers in PROJECT."
  (let ((projectile-switch-project-action 'projectile-save-project-buffers))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-kill-buffers (project)
  "Kill all buffers in PROJECT."
  (let ((projectile-switch-project-action 'projectile-kill-buffers))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-remove-known-project (project)
  "Remove PROJECT from the list of known projects."
  (projectile-remove-known-project project)
  (setq ivy--all-candidates
        (delete project ivy--all-candidates))
  (ivy--reset-state ivy-last))

(defun counsel-projectile-switch-project-action-compile (project)
  "Run PROJECT compliation command."
  (let ((projectile-switch-project-action
         (lambda ()
           (projectile-compile-project ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-configure (project)
  "Run PROJECT configure command."
  (let ((projectile-switch-project-action
         (lambda ()
           (projectile-configure-project ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-edit-dir-locals (project)
  "Edit PROJECT's dir-locals."
  (let ((projectile-switch-project-action 'projectile-edit-dir-locals))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-vc (project)
  "Open PROJECT in vc-dir / magit / monky."
  (let ((projectile-switch-project-action 'projectile-vc))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-run-shell (project)
  "Invoke `shell' from PROJECT's root."
  (let ((projectile-switch-project-action 'projectile-run-shell))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-run-eshell (project)
  "Invoke `eshell' from PROJECT's root."
  (let ((projectile-switch-project-action 'projectile-run-eshell))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-run-term (project)
  "Invoke `term' from PROJECT's root."
  (let ((projectile-switch-project-action
         (lambda ()
           (projectile-run-term nil))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-grep (project)
  "Search PROJECT with `grep'."
  (let ((projectile-switch-project-action 'counsel-projectile-grep))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-ag (project)
  "Search PROJECT with `ag'."
  (let ((projectile-switch-project-action 'counsel-projectile-ag))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-rg (project)
  "Search PROJECT with `rg'."
  (let ((projectile-switch-project-action 'counsel-projectile-rg))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-org-capture (project)
  "Capture into PROJECT."
  (let* ((from-buffer (ivy-state-buffer ivy-last))
         (projectile-switch-project-action `(lambda ()
                                              (counsel-projectile-org-capture ,from-buffer))))
    (counsel-projectile-switch-project-by-name project)))

(defun counsel-projectile-switch-project-action-org-agenda (project)
  "Open PROJECT agenda."
  (let ((projectile-switch-project-action 'counsel-projectile-org-agenda))
    (counsel-projectile-switch-project-by-name project)))

;;;###autoload
(defun counsel-projectile-switch-project ()
  "Switch project."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            (if counsel-projectile-remove-current-project
                (projectile-relevant-known-projects)
              projectile-known-projects)
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action counsel-projectile-switch-project-action
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'counsel-projectile-switch-project))

;;;; counsel-projectile

(counsel-projectile--defcustom-action
 'counsel-projectile
 '(1
   ("o" counsel-projectile-action
    "current window")
   ("j" counsel-projectile-action-other-window
    "other window")
   ("k" counsel-projectile-action-kill-buffer
    "kill buffer")
   ("x" counsel-projectile-action-file-extern
    "open file externally")
   ("r" counsel-projectile-action-file-root
    "open file as root")
   ("m" counsel-projectile-action-find-file-manually
    "find file manually")
   ("p" (lambda (_) (counsel-projectile-switch-project))
    "switch project"))
 'counsel-projectile)

(defvar counsel-projectile-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") (lambda ()
                                      (interactive)
                                      (counsel-projectile-action-kill-buffer (ivy-state-current ivy-last))))
    map)
  "Keymap for `counsel-projectile'.")

(defvar counsel-projectile--buffers nil
  "Stores a list of project buffers.")

(defvar counsel-projectile--non-visited-files nil
  "Stores a list of project files that are not currently
  visited by a buffer.")

(defun counsel-projectile--project-buffers-and-files (&rest _)
  ;; The ignored arguments are so that the function can be used as
  ;; collection function in `counsel-projectile'.
  "Return a list of buffers and non-visited files in the current
  project.  Buffers and files are separately sorted depending on
  `counsel-projectile-sort-buffers' and
  `counsel-projectile-sort-files', respectively."
  (let ((buffers (counsel-projectile--project-buffers))
        (files (projectile-current-project-files))
        (root (projectile-project-root))
        file sort-fn)
    ;; Remove files that are visited by a buffer:
    (dolist (buffer buffers files)
      (when (setq file (buffer-file-name (get-buffer buffer)))
        (setq files (remove (file-relative-name file root) files))))
    ;; Sort buffers and files depending on
    ;; `counsel-projectile-sort-buffers' and
    ;; `counsel-projectile-sort-files', respectively.
    ;; We need to do this here because matching will be done against
    ;; the variables `counsel-projectile--buffers' and
    ;; `counsel-projectile--non-visited-files', not against the
    ;; returned collection, so ivy's native sorting mechanism won't
    ;; work.
    (when (and counsel-projectile-sort-buffers
               (<= (length buffers) ivy-sort-max-size)
               (setq sort-fn (ivy--sort-function 'counsel-projectile-switch-to-buffer)))
      (setq buffers (sort (copy-sequence buffers) sort-fn)))
    (when (and counsel-projectile-sort-files
               (<= (length files) ivy-sort-max-size)
               (setq sort-fn (ivy--sort-function 'counsel-projectile-find-file)))
      (setq files (sort (copy-sequence files) sort-fn)))
    ;; Finally, bind `counsel-projectile--buffers' and
    ;; `counsel-projectile--non-visited-files' and return the whole
    ;; collection.
    (append (setq counsel-projectile--buffers buffers)
            (setq counsel-projectile--non-visited-files files))))

(defun counsel-projectile--matcher (regexp _candidates)
  "Return REGEXP-matching CANDIDATES for `counsel-projectile'.

Relies on `ivy--switch-buffer-matcher' for buffers and the
matcher specified in `counsel-projectile-find-file-matcher' for
files."
  (append (ivy--switch-buffer-matcher regexp counsel-projectile--buffers)
          (funcall counsel-projectile-find-file-matcher regexp counsel-projectile--non-visited-files)))

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

(defun counsel-projectile-action-kill-buffer (name)
  "Kill buffer named NAME."
  (if (member name counsel-projectile--buffers)
      (ivy--kill-buffer-action name)
    (message "This action only applies to buffers.")))
  
(defun counsel-projectile-action-find-file-manually (name)
  "Call `counsel-find-file' from default directory of buffer
directory of file named NAME."
  (if (member name counsel-projectile--buffers)
      (counsel-projectile-switch-to-buffer-action-find-file-manually name)
    (counsel-projectile-find-file-action-find-file-manually name)))

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

(defun counsel-projectile-transformer (str)
  "Fontifies modified, file-visiting buffers as well as non-visited files."
  (if (member str counsel-projectile--buffers)
      (counsel-projectile-switch-to-buffer-transformer str)
    (propertize str 'face 'ivy-virtual)))

;;;###autoload
(defun counsel-projectile (&optional arg)
  "Jump to a buffer or file in the current project.

With a prefix ARG, invalidate the cache first.

If not inside a project, call `counsel-projectile-switch-project'."
  (interactive "P")
  (if (not (projectile-project-p))
      (counsel-projectile-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (ivy-read (projectile-prepend-project-name "Load buffer or file: ")
              ;; We use a collection function so that it is called each
              ;; time the `ivy-state' is reset. This is needed for the
              ;; "kill buffer" action.
              #'counsel-projectile--project-buffers-and-files
              :matcher #'counsel-projectile--matcher
              :require-match t
              :action counsel-projectile-action
              :keymap counsel-projectile-map
              :caller 'counsel-projectile)))

(ivy-set-display-transformer
 'counsel-projectile
 'counsel-projectile-transformer)

;;;; counsel-projectile-mode

(defvar counsel-projectile-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map projectile-command-map)
    (define-key map (kbd "s r") 'counsel-projectile-rg)
    (define-key map (kbd "O c") 'counsel-projectile-org-capture)
    (define-key map (kbd "O a") 'counsel-projectile-org-agenda)
    (define-key map (kbd "SPC") 'counsel-projectile)
    map)
  "Keymap for Counesl-Projectile commands after `projectile-keymap-prefix'.")
(fset 'counsel-projectile-command-map counsel-projectile-command-map)

(defvar counsel-projectile-mode-map
  (let ((map (make-sparse-keymap))
        (projectile-command-keymap (where-is-internal 'projectile-command-map nil t)))
    (when projectile-command-keymap
      (define-key map projectile-command-keymap 'counsel-projectile-command-map))
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

;;;; provide

(provide 'counsel-projectile)

;;; counsel-projectile.el ends here
