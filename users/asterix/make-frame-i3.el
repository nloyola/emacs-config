;;; -*- lexical-binding: t; -*-

(defun nl/create-frame-in-workspace (title workspace)
  "Create a new frame with TITLE and move it to i3 WORKSPACE."
  (let ((frame (make-frame `((name . ,title)))))
    (run-at-time "0.5 sec" nil
                 (let ((title title)
                       (workspace workspace))
                   (lambda ()
                     (start-process "i3-move-frame" nil
                                    "i3-msg"
                                    (format "[title=\"%s\"] move container to workspace %s"
                                            title workspace)))))
    frame))
