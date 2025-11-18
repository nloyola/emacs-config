;;; -*- lexical-binding: t; -*-

(setq select-enable-clipboard t)
(setq select-enable-primary t)

(setq wl-copy-process nil)
(defun wl-copy (text)
  (setq wl-copy-process
        (make-process
         :name "wl-copy"
         :buffer nil
         :command '("wl-copy" "--foreground")
         :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (shell-command-to-string "wl-paste"))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(defun nl/main-frame-set-size-and-position ()
  "Set usable size for WSL."
  (interactive)
  (let* ((frame (selected-frame))
         (desired-width-in-chars 100)
         (desired-height-in-chars 36))
    (when (display-graphic-p)
      (set-frame-size frame desired-width-in-chars desired-height-in-chars)
      ;; this is useless in WSL
      ;;(set-frame-position frame 0 100)
      )))

(defun nl/new-frame ()
  "Create a new frame on the 4k display."
  (interactive)
  (let* ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer (current-buffer))
    (set-frame-position frame (+ (nl/monitor-pixel-width 1) 400) 120)
    (set-face-attribute 'default frame :font (nl/gui-fixed-font-normal))
    (set-frame-size frame
                    (* 120 (frame-char-width frame))
                    (* 70 (frame-char-height frame))
                    t)))

(defun nl/window-setup-hook ()
  (let ((frame (selected-frame)))
    (nl/main-frame-set-size-and-position)
    ;; (resume)
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook 'nl/window-setup-hook)

;; Search specified tree for the item with the specified key
;; http://stackoverflow.com/questions/11912027/emacs-lisp-search-anything in-a-nested-list
(defun tree-assoc (key tree)
  (when (consp tree)
    (cl-destructuring-bind (x . y) tree
  (if (eql x key) tree
    (or (tree-assoc key x) (tree-assoc key y))))))

;; adjust the font size based on the monitor's pixel height
;; http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution
(defun fontify-frame (&optional frame)
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if window-system
      (let ((monitor-pixel-height (nth 4 (assq 'geometry (frame-monitor-attributes frame))))
            (fixed-font (nl/gui-font nl/gui-fixed-font-name nl/default-font-size))
         (variable-font (nl/gui-font nl/gui-variable-font-name nl/default-variable-font-size)))
        (if (<= monitor-pixel-height 1080)
            (set-frame-parameter 'nil 'font fixed-font)
          (set-frame-parameter 'nil 'font fixed-font)))))

;;(push
;; 'fontify-frame after-make-frame-functions)

;;(run-with-idle-timer 2 t 'fontify-frame 'nil)

;; 0      1         2         3         4         5         6        7          8         9         0
;; 456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
