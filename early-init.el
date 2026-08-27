;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings that must be applied before `init.el' is loaded.

;;; Code:

;; Elpaca manages packages, so prevent Emacs from loading package.el first.
(setq package-enable-at-startup nil)

;; On WSL the GUI build is pgtk (Wayland), and Wayland forbids a client from
;; positioning its own window.  WSLg surfaces each Emacs frame as a real Win32
;; window, so after startup we ask Windows (via PowerShell + SetWindowPos) to
;; move the frame so its top-right corner sits at the display's top-right
;; corner.  See ~/.local/bin/emacs-top-right.ps1.
(when (getenv "WSL_DISTRO_NAME")
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when (eq (window-system) 'pgtk)
                (let ((ps "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
                      (script (concat "\\\\wsl.localhost\\"
                                      (getenv "WSL_DISTRO_NAME")
                                      (subst-char-in-string
                                       ?/ ?\\
                                       (expand-file-name
                                        "~/.local/bin/emacs-top-right.ps1")))))
                  (when (file-executable-p ps)
                    (start-process "emacs-top-right" nil ps
                                   "-NoProfile" "-ExecutionPolicy" "Bypass"
                                   "-File" script)))))))

;; Hyprland composites and blurs any surface that is actually see-through, so
;; the frost is bought entirely on this side.  `alpha-background' is the right
;; knob rather than `alpha': it dims only the pixels Emacs paints as background
;; and leaves glyphs fully opaque, so text stays crisp at values that would
;; make a whole-window `alpha' unreadable.  80 sits a touch more open than
;; alacritty's 0.85.
;;
;; This lives in `default-frame-alist' rather than on the initial frame so that
;; emacsclient frames inherit it, and here in early-init so the first frame is
;; mapped translucent instead of flashing opaque first.
(add-to-list 'default-frame-alist '(alpha-background . 80))

;;; early-init.el ends here
