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
;; This library tries to do something similar to `helm-projectile',
;; but using `ivy' instead of `helm'. The main function is
;; `counsel-projectile'.
;;
;;; Code:

(require 'counsel)
(require 'projectile)

;;; counsel-projectile-find-file

;;;###autoload
(defun counsel-projectile-find-file (&optional arg)
  "Jump to a project's file using completion.

Replacement for `projectile-find-file'.
With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read "Find file: "
            (projectile-current-project-files)
            :action
            (lambda (x)
              (with-ivy-window
                (find-file
                 (projectile-expand-root x))))
            :require-match t
            :caller 'counsel-projectile-find-file)
  (run-hooks 'projectile-find-file-hook))

(ivy-set-actions
 'counsel-projectile-find-file
 '(("j" (lambda (x)
          (with-ivy-window
            (find-file-other-window
             (projectile-expand-root x))))
    "other window")))

;;; counsel-projectile-find-dir

;;;###autoload
(defun counsel-projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (ivy-read "Find dir: "
            (if projectile-find-dir-includes-top-level
                (append '("./") (projectile-current-project-dirs))
              (projectile-current-project-dirs))
            :action
            (lambda (x)
              (with-ivy-window
                (dired
                 (projectile-expand-root x))))
            :require-match t
            :caller 'counsel-projectile-find-dir)
  (run-hooks 'projectile-find-dir-hook))

(ivy-set-actions
 'counsel-projectile-find-dir
 '(("j" (lambda (x)
          (with-ivy-window
            (dired-other-window
             (projectile-expand-root x))))
    "other window")))

;;; counsel-projectile-switch-to-buffer

;;;###autoload
(defun counsel-projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (ivy-read "Switch-to-buffer: "
            (-remove-item (buffer-name (current-buffer))
                          (projectile-project-buffer-names))
            :action
            (lambda (x)
              (with-ivy-window
                (switch-to-buffer x)))
            :require-match t
            :caller 'counsel-projectile-switch-to-buffer))

(ivy-set-actions
 'counsel-projectile-switch-to-buffer
 '(("j" (lambda (x)
          (with-ivy-window
            (switch-to-buffer-other-window x)))
    "other window")))

;;; counsel-projectile

;;;###autoload
(defun counsel-projectile (&optional arg)
  "Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (ivy-read "Switch to project: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action
            (lambda (dir)
              (projectile-switch-project-by-name dir arg))
            :require-match t
            :caller 'counsel-projectile))

(ivy-set-actions
 'counsel-projectile
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


(provide 'counsel-projectile)

;;; counsel-projectile.el ends here
