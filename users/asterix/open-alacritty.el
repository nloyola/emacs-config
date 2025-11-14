;;; -*- lexical-binding: t; -*-
;;;

(defun nl/open-alacritty-here ()
  "Open a new Alacritty terminal window in the current buffer's directory, detached from Emacs."
  (interactive)
  (let* ((dir (cond
               ((derived-mode-p 'dired-mode) (dired-current-directory))
               ((buffer-file-name) (file-name-directory (buffer-file-name)))
               (t default-directory))))
    (when dir
      (call-process
       "setsid" ;; fully detaches from Emacs process group
       nil 0 nil
       "alacritty" "--working-directory" dir))))
