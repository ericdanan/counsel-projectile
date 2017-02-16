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

(defun counsel-projectile--file-list (&optional no-buffer)
  "Return a list of files for the current project.

Like `projectile-current-project-files', but fontifies
non-visited file names with the `ivy-virtual' face.  With optional
argument NO-BUFFER, only list non-visited files."
  (let ((root (projectile-project-root)))
    (cl-loop
     for name in (projectile-current-project-files)
     for file = (expand-file-name name root)
     if (not (get-file-buffer file))
     collect (propertize name 'face 'ivy-virtual)
     else
     unless no-buffer
     collect name)))

(defun counsel-projectile--find-file-action (file &optional other-window)
  "Find FILE and run `projectile-find-file-hook'."
  (funcall (if other-window
               'find-file-other-window
             'find-file)
           (projectile-expand-root file))
  (run-hooks 'projectile-find-file-hook))

(defun counsel-projectile--find-file-other-window-action (file)
  "Find FILE in another window and run
`projectile-find-file-hook'."
  (counsel-projectile--find-file-action file t))

;;;###autoload
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

Replacement for `projectile-find-file'.  With a prefix ARG
invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (counsel-projectile--file-list)
            :matcher #'counsel--find-file-matcher
            :require-match t
            :keymap counsel-projectile-map
            :action #'counsel-projectile--find-file-action
            :caller 'counsel-projectile-find-file))

(ivy-set-actions
 'counsel-projectile-find-file
 '(("j" counsel-projectile--find-file-other-window-action
    "other window")))

;;; counsel-projectile-find-dir

(defun counsel-projectile--dir-list ()
  "Return a list of files for the current project."
  (if projectile-find-dir-includes-top-level
      (append '("./") (projectile-current-project-dirs))
    (projectile-current-project-dirs)))

(defun counsel-projectile--find-dir-action (dir &optional other-window)
  "Visit DIR with dired and run `projectile-find-dir-hook'."
  (funcall (if other-window
               'dired-other-window
             'dired)
           (projectile-expand-root dir))
  (run-hooks 'projectile-find-dir-hook))

(defun counsel-projectile--find-dir-other-window-action (dir)
  "Visit DIR with dired in another window and run
`projectile-find-dir-hook'."
  (counsel-projectile--find-dir-action dir t))

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
            :action #'counsel-projectile--find-dir-action
            :caller 'counsel-projectile-find-dir))

(ivy-set-actions
 'counsel-projectile-find-dir
 '(("j" counsel-projectile--find-dir-other-window-action
    "other window")))

;;; counsel-projectile-switch-to-buffer

(defun counsel-projectile--buffer-list ()
  "Get a list of project buffer names.

Like `projectile-project-buffer-names', but propertize buffer
names as in `ivy--buffer-list'."
  (ivy--buffer-list "" nil
                    (lambda (x)
                      (member (car x) (projectile-project-buffer-names)))))

(defun counsel-projectile--switch-buffer-action (buffer &optional other-window)
  "Switch to BUFFER.

BUFFER may be a string or nil."
  (cond
   ((zerop (length buffer))
    (switch-to-buffer ivy-text nil 'force-same-window))
   (other-window
    (switch-to-buffer-other-window buffer))
   (t
    (switch-to-buffer buffer nil 'force-same-window))))

(defun counsel-projectile--switch-buffer-other-window-action (buffer)
  "Switch to BUFFER in other window.

BUFFER may be a string or nil."
  (counsel-projectile--switch-buffer-action buffer t))

;;;###autoload
(defun counsel-projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to buffer: ")
            (counsel-projectile--buffer-list)
            :matcher #'ivy--switch-buffer-matcher
            :require-match t
            :keymap counsel-projectile-map
            :action #'counsel-projectile--switch-buffer-action
            :caller 'counsel-projectile-switch-to-buffer))

(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer
 'ivy-switch-buffer-transformer)

(ivy-set-actions
 'counsel-projectile-switch-to-buffer
 '(("j" counsel-projectile--switch-buffer-other-window-action
    "other window")))

;;; counsel-projectile-ag

;;;###autoload
(defun counsel-projectile-ag (&optional options)
  "Ivy version of `projectile-ag'."
  (interactive)
  (if (projectile-project-p)
      (let* ((options
              (if current-prefix-arg
                  (read-string "options: ")
                options))
             (ignored
              (unless (eq (projectile-project-vcs) 'git)
                ;; ag supports git ignore files
                (append
                 (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
                 (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))))
             (options
              (concat options " "
                      (mapconcat (lambda (i)
                                   (concat "--ignore " (shell-quote-argument i)))
                                 ignored
                                 " "))))
        (counsel-ag nil
                    (projectile-project-root)
                    options
                    (projectile-prepend-project-name "ag")))
    (user-error "You're not in a project")))

;;; counsel-projectile-rg

;;;###autoload
(defun counsel-projectile-rg (&optional options)
  "Ivy version of `projectile-rg'."
  (interactive)
  (if (projectile-project-p)
      (let* ((options
              (if current-prefix-arg
                  (read-string "options: ")
                options))
             (ignored
              (unless (eq (projectile-project-vcs) 'git)
                ;; rg supports git ignore files
                (append
                 (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
                 (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))))
             (options
              (concat options " "
                      (mapconcat (lambda (i)
                                   (concat "--ignore-file " (shell-quote-argument i)))
                                 ignored
                                 " "))))
        (counsel-rg nil
                    (projectile-project-root)
                    options
                    (projectile-prepend-project-name "rg")))
    (user-error "You're not in a project")))


;;; counsel-projectile-switch-project

;;;###autoload
(defun counsel-projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.

Invokes the command referenced by
`projectile-switch-project-action' on switch.  With a prefix ARG
invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action (lambda (dir)
                      (projectile-switch-project-by-name dir arg))
            :require-match t
            :caller 'counsel-projectile-switch-project))

(ivy-set-actions
 'counsel-projectile-switch-project
 '(("f" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-find-file))
            (projectile-switch-project-by-name dir arg)))
    "find file")
   ("d" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-find-dir))
            (projectile-switch-project-by-name dir arg)))
    "find directory")
   ("b" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-switch-to-buffer))
            (projectile-switch-project-by-name dir arg)))
    "switch to buffer")
   ("s" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-save-project-buffers))
            (projectile-switch-project-by-name dir arg)))
    "save all buffers")
   ("k" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-kill-buffers))
            (projectile-switch-project-by-name dir arg)))
    "kill all buffers")
   ("r" (lambda (dir)
          (let ((projectile-switch-project-action
                 'projectile-remove-current-project-from-known-projects))
            (projectile-switch-project-by-name dir arg)))
    "remove from known projects")
   ("l" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-edit-dir-locals))
            (projectile-switch-project-by-name dir arg)))
    "edit dir-locals")
   ("g" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-vc))
            (projectile-switch-project-by-name dir arg)))
    "open in vc-dir / magit / monky")
   ("e" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-run-eshell))
            (projectile-switch-project-by-name dir arg)))
    "start eshell")
   ("a" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-ag))
            (projectile-switch-project-by-name dir arg)))
    "search with ag")))

;;; counsel-projectile

(defun counsel-projectile--global-list ()
  "Get a list of project buffers and files."
  (append
   (mapc (lambda (buffer)
           (add-text-properties 0 1 '(type buffer) buffer))
         (counsel-projectile--buffer-list))
   (mapc (lambda (file)
           (add-text-properties 0 1 '(type file) file))
         (counsel-projectile--file-list t))))

(defun counsel-projectile--transformer (str)
  "Fontifies modified, file-visiting buffers.

Relies on `ivy-switch-buffer-transformer'."
  (if (eq (get-text-property 0 'type str) 'buffer)
      (ivy-switch-buffer-transformer str)
    str))

(defun counsel-projectile--matcher (regexp candidates)
  "Return REGEXP-matching CANDIDATES.

Relies on `ivy--switch-buffer-matcher` and
`counsel--find-file-matcher'."
  (let ((buffers (cl-remove-if-not (lambda (name)
                                     (eq (get-text-property 0 'type name) 'buffer))
                                   candidates))
        (files (cl-remove-if-not (lambda (name)
                                   (eq (get-text-property 0 'type name) 'file))
                                 candidates)))
    (append (ivy--switch-buffer-matcher regexp buffers)
            (counsel--find-file-matcher regexp files))))

(defun counsel-projectile--action (name &optional other-window)
  "Switch to buffer or find file named NAME."
  (let ((type (get-text-property 0 'type name)))
    (cond
     ((eq type 'file)
      (counsel-projectile--find-file-action name other-window))
     ((eq type 'buffer)
      (counsel-projectile--switch-buffer-action name other-window)))))

(defun counsel-projectile--other-window-action (name)
  "Switch to buffer or find file named NAME in another window."
  (counsel-projectile--action name t))

;;;###autoload
(defun counsel-projectile (&optional arg)
  "Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (if (not (projectile-project-p))
      (counsel-projectile-switch-project arg)
    (projectile-maybe-invalidate-cache arg)
    (ivy-read (projectile-prepend-project-name "Load buffer or file: ")
              (counsel-projectile--global-list)
              :matcher #'counsel-projectile--matcher
              :require-match t
              :keymap counsel-projectile-map
              :action #'counsel-projectile--action
              :caller 'counsel-projectile)))

(ivy-set-display-transformer
 'counsel-projectile
 'counsel-projectile--transformer)

(ivy-set-actions
 'counsel-projectile
 '(("j" counsel-projectile--other-window-action
    "other window")))

;;; key bindings

;;;###autoload
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "SPC") #'counsel-projectile)))

(defun counsel-projectile-commander-bindings ()
  (def-projectile-commander-method ?f
    "Find file in project."
    (counsel-projectile-find-file))
  (def-projectile-commander-method ?d
    "Find directory in project."
    (counsel-projectile-find-dir))
  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (counsel-projectile-switch-to-buffer))
  (def-projectile-commander-method ?A
    "Search project files with ag."
    (counsel-projectile-ag))
  (def-projectile-commander-method ?s
    "Switch project."
    (counsel-projectile-switch-project)))

(defun counsel-projectile-toggle (toggle)
  "Toggle Ivy version of Projectile commands."
  (if (> toggle 0)
      (progn
        (when (eq projectile-switch-project-action #'projectile-find-file)
          (setq projectile-switch-project-action #'counsel-projectile))
        (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
        (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
        (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project)
        (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-ag)
        (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
        (counsel-projectile-commander-bindings))
    (progn
      (when (eq projectile-switch-project-action #'counsel-projectile)
        (setq projectile-switch-project-action #'projectile-find-file))
      (define-key projectile-mode-map [remap projectile-find-file] nil)
      (define-key projectile-mode-map [remap projectile-find-dir] nil)
      (define-key projectile-mode-map [remap projectile-switch-project] nil)
      (define-key projectile-mode-map [remap projectile-ag] nil)
      (define-key projectile-mode-map [remap projectile-switch-to-buffer] nil)
      (projectile-commander-bindings))))

;;;###autoload
(defun counsel-projectile-on ()
  "Turn on counsel-projectile key bindings."
  (interactive)
  (message "Turn on counsel-projectile key bindings")
  (counsel-projectile-toggle 1))

;;;###autoload
(defun counsel-projectile-off ()
  "Turn off counsel-projectile key bindings."
  (interactive)
  (message "Turn off counsel-projectile key bindings")
  (counsel-projectile-toggle -1))


(provide 'counsel-projectile)

;;; counsel-projectile.el ends here
