;;; counsel-projectile.el --- Ivy integration for Projectile

;; Copyright (C) 2016 Eric Danan

;; Author: Eric Danan
;; URL: https://github.com/ericdanan/counsel-projectile
;; Created: 2016-04-11
;; Keywords: project, convenience
;; Version: 0.1
;; Package-Requires: ((counsel "0.7.0") (projectile "0.13.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Projectile has native support for using ivy as its completion
;; system. Counsel-projectile provides further ivy integration into
;; projectile by taking advantage of ivy's mechanism to select from a
;; list of actions and/or apply an action without leaving the comlpetion
;; session. It is inspired by helm-projectile. See the README for more details.
;;
;;; Code:

(require 'counsel)
(require 'projectile)

;;; counsel-projectile-map

(defun counsel-projectile-drop-to-switch-project ()
  "For use in minibuffer maps. Quit and call `counsel-projectile-switch-project'."
  (interactive)
  (ivy-quit-and-run
   (counsel-projectile-switch-project)))

(defvar counsel-projectile-drop-to-switch-project-binding "M-SPC"
  "Key binding for `counsel-projectile-drop-to-switch-project' in `counsel-projectile-map'.")

(defvar counsel-projectile-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (kbd counsel-projectile-drop-to-switch-project-binding)
      'counsel-projectile-drop-to-switch-project)
    map)
  "Keymap used in the minibuffer.")

;;; counsel-projectile-find-file

;;;###autoload
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

Replacement for `projectile-find-file'.
With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (projectile-current-project-files)
            :action (lambda (x)
                      (find-file (projectile-expand-root x)))
            :require-match t
            :keymap counsel-projectile-map
            :caller 'counsel-projectile-find-file)
  (run-hooks 'projectile-find-file-hook))

(ivy-set-actions
 'counsel-projectile-find-file
 '(("j" (lambda (x)
          (find-file-other-window (projectile-expand-root x)))
    "other window")))

;;; counsel-projectile-find-dir

;;;###autoload
(defun counsel-projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find dir: ")
            (if projectile-find-dir-includes-top-level
                (append '("./") (projectile-current-project-dirs))
              (projectile-current-project-dirs))
            :action (lambda (x)
                      (dired (projectile-expand-root x)))
            :require-match t
            :keymap counsel-projectile-map
            :caller 'counsel-projectile-find-dir)
  (run-hooks 'projectile-find-dir-hook))

(ivy-set-actions
 'counsel-projectile-find-dir
 '(("j" (lambda (x)
          (dired-other-window (projectile-expand-root x)))
    "other window")))

;;; counsel-projectile-switch-to-buffer

(defvar counsel-projectile--virtual-buffers nil
  "Store the project virtual buffers alist.")

(defun counsel-projectile--virtual-buffers ()
  "Adapted from `ivy--virtual-buffers'."
  (let (virtual-buffers filename)
    (dolist (name (projectile-current-project-files))
      (and (not (equal name ""))
           (null (get-file-buffer (setq filename (projectile-expand-root name))))
           (not (assoc name virtual-buffers))
           (push (cons name filename) virtual-buffers)))
    (when virtual-buffers
      (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
                           'face 'ivy-virtual
                           (car comp)))
      (setq counsel-projectile--virtual-buffers (nreverse virtual-buffers))
      (mapcar #'car counsel-projectile--virtual-buffers))))

(defun counsel-projectile--buffer-list (&optional virtual)
  "Adapted from `ivy--buffer-list'."
  (delete-dups
   (append
    (ivy--buffer-list "" nil (lambda (x)
                                (member (car x) (projectile-project-buffer-names))))
    (and virtual
         (counsel-projectile--virtual-buffers)))))

(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer 'ivy-switch-buffer-transformer)

(defun counsel-projectile--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (let ((ivy--virtual-buffers counsel-projectile--virtual-buffers)
        (ivy-views nil))
    (ivy--switch-buffer-action buffer)))

(defun counsel-projectile--switch-buffer-other-window-action (buffer)
  "Switch to BUFFER in other window.
BUFFER may be a string or nil."
  (let ((ivy--virtual-buffers counsel-projectile--virtual-buffers)
        (ivy-views nil))
    (ivy--switch-buffer-other-window-action buffer)))

;;;###autoload
(defun counsel-projectile-switch-to-buffer (&optional virtual)
  "Switch to a project buffer.
If optional argument VIRTUAL is non-nil, add project files as virtual buffers."
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to buffer: ")
            (counsel-projectile--buffer-list virtual)
            :matcher #'ivy--switch-buffer-matcher
            :action #'counsel-projectile--switch-buffer-action
            :require-match t
            :keymap counsel-projectile-map
            :caller 'counsel-projectile-switch-to-buffer))

(ivy-set-actions
 'counsel-projectile-switch-to-buffer
 '(("j" counsel-projectile--switch-buffer-other-window-action
    "other window")))

;;; counsel-projectile-find-file-or-buffer

;;;###autoload
(defun counsel-projectile-find-file-or-buffer (&optional arg)
  "Visit a project file or buffer.

With a prefix ARG invalidates the cache first."
  (interactive)
  (projectile-maybe-invalidate-cache arg)
  (counsel-projectile-switch-to-buffer t))

;;; counsel-projectile-switch-project

;;;###autoload
(defun counsel-projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of `projectile-switch-project-action.'"
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
    "start eshell")))

;;; counsel-projectile

;;;###autoload
(defun counsel-projectile (&optional arg)
  "Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (if (projectile-project-p)
        (counsel-projectile-find-file-or-buffer arg)
    (counsel-projectile-switch-project)))

;;; key bindings

;;;###autoload
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "SPC") #'counsel-projectile)))

(defun counsel-projectile-commander-bindings ()
  (def-projectile-commander-method ?f
    "Find file in project."
    (counsel-projectile-find-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (counsel-projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (counsel-projectile-find-dir))

  (def-projectile-commander-method ?s
    "Switch project."
    (counsel-projectile-switch-project)))

(defun counsel-projectile-toggle (toggle)
  "Toggle Ivy version of Projectile commands."
  (if (> toggle 0)
      (progn
        (when (eq projectile-switch-project-action #'projectile-find-file)
          (setq projectile-switch-project-action #'counsel-projectile-find-file-or-buffer))
        (define-key projectile-command-map (kbd "f") #'counsel-projectile-find-file)
        (define-key projectile-command-map (kbd "d") #'counsel-projectile-find-dir)
        (define-key projectile-command-map (kbd "p") #'counsel-projectile-switch-project)
        (define-key projectile-command-map (kbd "b") #'counsel-projectile-switch-to-buffer)
        (counsel-projectile-commander-bindings))
    (progn
      (when (eq projectile-switch-project-action #'counsel-projectile-find-file-or-buffer)
        (setq projectile-switch-project-action #'projectile-find-file))
      (define-key projectile-command-map (kbd "f") #'projectile-find-file)
      (define-key projectile-command-map (kbd "d") #'projectile-find-dir)
      (define-key projectile-command-map (kbd "p") #'projectile-switch-project)
      (define-key projectile-command-map (kbd "b") #'projectile-switch-to-buffer)
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
