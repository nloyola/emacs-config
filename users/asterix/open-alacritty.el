;;; -*- lexical-binding: t; -*-
;;;

(defun nl/open-alacritty--detached (dir)
  "Open a detached Alacritty terminal window in DIR."
  (let ((process-environment (copy-sequence process-environment)))
    ;; Do not let the spawned shell think it is running inside Emacs.  The zsh
    ;; config intentionally uses a minimal prompt when INSIDE_EMACS is set.
    (setenv "INSIDE_EMACS" nil)
    (setenv "EMACS" nil)
    (call-process
     "setsid" ;; fully detaches from Emacs process group
     nil 0 nil
     "env" "-u" "INSIDE_EMACS" "-u" "EMACS"
     "alacritty" "--working-directory" dir)))

(defun nl/open-alacritty-here ()
  "Open a new Alacritty terminal window in the current buffer's directory, detached from Emacs."
  (interactive)
  (let* ((dir (cond
               ((derived-mode-p 'dired-mode) (dired-current-directory))
               ((buffer-file-name) (file-name-directory (buffer-file-name)))
               (t default-directory))))
    (when dir
      (nl/open-alacritty--detached dir))))

(defun nl/open-alacritty-project-root ()
  "Open a new Alacritty terminal window in the current project root, detached from Emacs."
  (interactive)
  (require 'projectile)
  (let ((dir (projectile-project-root)))
    (when dir
      (nl/open-alacritty--detached dir))))
