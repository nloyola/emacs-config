;;; -*- lexical-binding: t; -*-
;;;

(defun nl/open-alacritty-here ()
  "Open a new Alacritty terminal window in the current buffer's directory."
  (interactive)
  (let* ((dir (cond
               ((derived-mode-p 'dired-mode) (dired-current-directory))
               ((buffer-file-name) (file-name-directory (buffer-file-name)))
               (t default-directory))))
    (when dir
      (start-process "alacritty" nil "alacritty" "--working-directory" dir))))
