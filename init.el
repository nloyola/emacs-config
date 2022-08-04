;;; init.el --- Emacs Initialization File

;;; Commentary:
;;; initialization

;;; Code:

;; For debugging startup time only
;; (with-current-buffer (messages-buffer)
;;   (goto-char (point-max))
;;   (switch-to-buffer (current-buffer)))

(defun nl/display-startup-time ()
  (let ((elapsed
         (float-time
          (time-subtract (current-time) emacs-start-time))))
    (message "Emacs loaded in %.3f seconds with %d garbage collections"
             elapsed gcs-done)))

(add-hook 'emacs-startup-hook #'nl/display-startup-time)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil)

(defun nl/after-init ()
  (setq file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 200000000
        gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'after-init-hook `nl/after-init t)

(setq comp-deferred-compilation t)

;; Turn off mouse interface early in startup to avoid momentary display
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode -1)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; when tramp is slow
;;
;; - not working really
;;(setq projectile-mode-line "Projectile")

;;; Set up package
(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(setq package-archive-priorities
      '(;;("melpa-stable" . 10)
        ;; ("marmalade" . 7)
        ("gnu"       . 5)
        ("org"       . 7)
        ("melpa"     . 10)))

(package-initialize)

;; Using HTTPS for downloading packages, make sure HTTPS is not going through a proxy.
;; (setenv "https_proxy" "")
;; (setenv "http_proxy" "")

;;; Bootstrap use-package
(setq-default use-package-always-ensure t         ; Auto-download package if not exists
              use-package-always-defer t          ; Always defer load package to speed up startup time
              use-package-expand-minimally nil    ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; use only for debugging startup time
(setq use-package-verbose t)               ; report loading details)

;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(require 'diminish)
(require 'bind-key)
;;(setq use-package-verbose nil)
(setq use-package-verbose t)

(use-package pl
  :load-path "~/.config/.emacs.d/lisp"
  :commands pl-parse
  )

;; see http://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code
(defmacro with-timer (&rest forms)
  "Run the given FORMS, counting and displaying the elapsed time."
  (declare (indent 0))
  (let ((nowvar (make-symbol "now"))
        (body `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (prog1 ,body
         (let ((elapsed (float-time (time-subtract (current-time) ,nowvar))))
           (when (> elapsed 0.001)
             (message "spent (%.3fs)" elapsed)))))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(defun nl/show-messages-on-startup ()
  "Show the *Messages* buffer after starting Emacs."
  (setq initial-buffer-choice (lambda () (get-buffer "*Messages*"))))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(setq user-full-name "Nelson Loyola"
      user-mail-address "nloyola@gmail.com")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq comp-async-report-warnings-errors nil)

(defvar nl/gui-fixed-font-name "FiraCode Nerd Font")
;;(defvar nl/gui-fixed-font-name "CaskaydiaCove Nerd Font Mono")

;; (defvar nl/gui-fixed-font-name "IBM Plex Mono Medium")
;; (defvar nl/gui-variable-font-name "DejaVu Sans")

;;(defvar nl/gui-variable-font-name "GoMono Nerd Font")
(defvar nl/gui-variable-font-name "Go")

(defconst nl/gui-fixed-font-size-normal "10")
(defconst nl/gui-fixed-font-size-large "18")

(defconst nl/gui-variable-font-size-normal "12")
(defconst nl/gui-variable-font-size-large "18")

(defvar nl/gui-current-fixed-font-size nl/gui-fixed-font-size-normal)
(defvar nl/gui-current-variable-font-size nl/gui-variable-font-size-normal)

(defun nl/gui-font (font-name font-size)
  (concat font-name "-" font-size))

(defun nl/gui-fixed-font-normal ()
  (nl/gui-font nl/gui-fixed-font-name nl/gui-fixed-font-size-normal))

(defun nl/gui-fixed-font-large ()
  (nl/gui-font nl/gui-fixed-font-name nl/gui-fixed-font-size-large))

(defun nl/gui-variable-font-normal ()
  (nl/gui-font nl/gui-variable-font-name nl/gui-variable-font-size-normal))

(defun nl/gui-variable-font-large ()
  (nl/gui-font nl/gui-variable-font-name nl/gui-variable-font-size-large))

(defun nl/set-fonts (frame)
  "Set the desired fonts for a frame. FRAME can be nil."
  (let ((fixed-font (nl/gui-fixed-font-normal)))
    (set-face-font 'default fixed-font)
    (set-face-font 'fixed-pitch fixed-font))
  (set-face-font 'variable-pitch (nl/gui-variable-font-normal))
  (set-face-attribute 'font-lock-comment-face nil :weight 'semi-bold :slant 'italic)
  ;;(set-face-background 'region (doom-darken 'green 0.2))

  (when frame
    ;;(set-face-attribute 'default frame :font nl/gui-fixed-font-name)
    (set-face-attribute 'italic frame :font nl/gui-fixed-font-name :weight 'normal :slant 'italic)
    (set-face-attribute 'bold frame :font nl/gui-fixed-font-name :weight 'bold :weight 'normal)
    (set-face-attribute 'bold-italic frame :font nl/gui-fixed-font-name :weight 'bold :slant 'italic)
    (set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Fira Code Retina"))
    )

  (set-face-font 'mode-line (nl/gui-variable-font-normal))
  (set-face-font 'mode-line-buffer-id (nl/gui-variable-font-normal))
  (set-face-font 'mode-line-emphasis (nl/gui-variable-font-normal))
  (set-face-font 'mode-line-highlight (nl/gui-variable-font-normal))
  (set-face-font 'mode-line-inactive (nl/gui-variable-font-normal))
  )

(defun nl/after-make-frame (frame)
  ;; disable the toolbar when in daemon mode
  ;;
  ;; https://emacs.stackexchange.com/questions/39359/tool-bar-in-emacsclient
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
        (tool-bar-mode -1)
        (nl/set-fonts frame)
        ))))

(add-hook 'after-make-frame-functions 'nl/after-make-frame t)

(use-package emacs
  :hook
  ;; Make completion buffers disappear after 15 seconds.
  (completion-setup . (lambda ()
                        (run-at-time 15 nil
                                     (lambda ()
                                       (delete-windows-on "*Completions*")))))

  ;; Remove trailing whitespace
  (before-save . delete-trailing-whitespace)
  :bind
  ("C-z" . nil)     ;; I never want to suspend the frame
  )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

;; Set the default major mode to =text-mode=. By default it's =fundamental= mode which is
;; not what we want. Also, use =text-mode= for the scratch buffer.

(setq default-major-mode 'text-mode
      initial-major-mode 'text-mode)

;; Don't scroll to middle of the page. Also, scroll line by line, without
;; scrolloff and try to keep point at the same visual place when
;; scrolling by page.
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)
(setq redisplay-dont-pause t
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 10
      scroll-preserve-screen-position t)

;; Level of decoration {1 - 3}, t = max.
(setq font-lock-maximum-decoration t)

;; For symlinks, automatically follow the link and visit the real file instead.
(setq vc-follow-symlinks nil)

;; Make searches case insensitive.
(setq-default case-fold-search t)

;; Autosave files are created between saves after a sufficient timeout in
;; the current directory for crash detection, they begin and end with
;; =#=.  Change this location to the emacs directory.
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms `((".*" "~/.emacs.d/autosave/" t))
      backup-directory-alist `(("." . ,(concat user-emacs-directory "autosave"))))

;;Set line wrap at column 100.
(setq fill-column 100)

;; Replace =yes or no= prompt with =y or n= prompt.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use UTF-8 everywhere.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; Highlight matches in query-replace mode.
(setq query-replace-highlight t)

;; Use visual line mode to display long lines.
(global-visual-line-mode -1)

;;Revert these files without asking.
(setq revert-without-query '(".*"))

;; Middle-clicking is nice to paste, however it should not adjust point
;; and paste at the then adjusted point.
(setq mouse-yank-at-point t)

;; Save clipboard data of other programs in the kill ring when possible.
(setq save-interprogram-paste-before-kill t)

;; Set environment variable for shells.
(setenv "PAGER" "cat")

;; Configure =next-buffer= and =previous-buffer= to only visit file
;; buffers (has to be called for each frame):
(set-frame-parameter (selected-frame) 'buffer-predicate #'buffer-file-name)

;; These are taken from
;; https://github.com/patrickt/emacs/blob/master/init.el:
(setq
 kill-whole-line t                      ; Lets C-k delete the whole line
 ;;default-directory "~/src/"             ; My code lives here
 enable-recursive-minibuffers t         ; don't freak out if I use the minibuffer twice
 sentence-end-double-space nil          ; are you kidding me
 confirm-kill-processes nil             ; don't when quitting
 )

(setq-default
 cursor-type 'box
 indent-tabs-mode nil
 cursor-in-non-selected-windows nil)

;; Cursor Movement
(setq auto-window-vscroll nil)

;; Turn off auto-save.
(setq auto-save-default nil)

;; Don't make any backup files.
(setq make-backup-files nil)

;; Get rid of the menu bar. Tool bar and scroll bars are disabled in
;; ~init.el~..
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Turn off the blinking cursor.
(blink-cursor-mode -1)

;; Don't use dialog boxes
(setq use-dialog-box nil)

;; Don't want an audible bell.
(setq visible-bell t)

;; Display the running program and the selected buffer in the frame title.
(setq frame-title-format
      '("" invocation-name ": " (:eval (replace-regexp-in-string
                                        "^ +" "" (buffer-name)))))
;; Don't add new lines past end of file, and indicate unused lines at the
;; end of the window with a small image in the left fringe.
(setq next-line-add-newlines nil)
(setq-default indicate-empty-lines t)

;; Add =\n= to end of file if required.
(setq require-final-newline t)

;; Eshell
(setq eshell-history-size 100000)

;; Follow Buffer

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Automatically cycle through the highlighting faces listed in
;; ~hi-lock-face-defaults~ instead of bothering the user to pick a face
;; each time.
(setq hi-lock-auto-select-face t)

;; History
(setq history-delete-duplicates t)

;; Use the directory name to make buffer names unique.
(setq uniquify-buffer-name-style 'forward)

(global-so-long-mode 1)
(defun nl/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun nl/consult-compile ()
  "Use Consult to choose a compile command."
  (interactive)
  (let ((selected-command
         (completing-read "Select a compile command: " compile-history)))
    ;; move this command to the front of the history
    (setq compile-history (remove selected-command compile-history))
    (add-to-list 'compile-history selected-command)
    (compile selected-command)))

(defun nl/consult-async-shell-command ()
  (interactive)
  (let ((selected-command
         (completing-read "Select a shell command: " shell-command-history)))
    (async-shell-command selected-command)))

;; (defun nl/counsel-git-files ()
;;   (interactive)
;;   (let ((counsel-fzf-cmd "git ls-files | fzf -f \"%s\""))
;;     (counsel-fzf)))

(global-set-key (kbd "M-%")           'query-replace-regexp)
;;(global-set-key "\C-x\C-e"          'compile)
(global-set-key (kbd "C-S-s")         'isearch-forward)
(global-set-key (kbd "C-x C-n")       'next-error)
(global-set-key (kbd "C-x k")         'nl/kill-this-buffer)
(global-set-key (kbd "M-f")           'forward-to-word)
(global-set-key (kbd "M-B")           'backward-to-word)

(global-set-key (kbd "<f1>")          'indent-for-tab-command)
(global-set-key (kbd "S-<f1>")        'indent-region)
(global-set-key (kbd "<f2>")          '(lambda () (interactive) (save-some-buffers t)))
(global-set-key (kbd "S-<f2>")        '(lambda () (interactive) (revert-buffer t t)))
;;(global-set-key (kbd "S-<f3>")        'helm-projectile-rg)
;;(global-set-key (kbd "M-S-<f3>")      'counsel-rg)
(global-set-key [f5]                  'nl/consult-compile)
(global-set-key (kbd "S-<f5>")        'toggle-truncate-lines)
(global-set-key (kbd "<f8>")          'window-toggle-side-windows)
(global-set-key (kbd "S-<f11>")       'eval-region)
(global-set-key (kbd "C-S-<f11>")     'align-regexp)
;;(global-set-key (kbd "C-c o")         'nl/counsel-git-files)

(use-package recentf
  :init (recentf-mode 1)
  :custom
  (recentf-save-file "~/.emacs.d/etc/recentf")
  (recentf-max-saved-items 100))

(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/etc/savehist"
      history-length 150)
(savehist-mode 1)

(use-package saveplace
  :custom
  (save-place-file (locate-user-emacs-file "etc/saveplace" "places"))
  (save-place-forget-unreadable-files nil)
  (save-place-ignore-files-regexp "\\(?:COMMIT_EDITMSG\\|svn-commit\\.tmp\\|config\\.org\\)$")
  ;; activate it for all buffers
  :init
  ;;(setq-default save-place t)
  (save-place-mode t))

(defvar nl/side-window-parameters
  '(window-parameters . ((no-other-window . nil)
                         (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

;; (setq display-buffer-alist '())

;; (defun nl/display-buffer-debug(buf-name action)
;;   (message "%s" buf-name)
;;   (numberp (string-match "\\(?:\\*\\(?:[Hh]elp\\|grep\\|Warnings\\|Completions\\|xref\\)\\)\\*\\)\\|\\(?:\\(?:HELM.*\\|helm.*\\)\\)" buf-name)))

(add-to-list 'display-buffer-alist
             '("\\(?:\\*\\(?:grep\\|Find\\|Warnings\\|xref\\)\\*\\)\\|\\(?:\\(?:HELM.*\\|helm.*\\)\\)"
               display-buffer-in-side-window
               (window-height . 0.15)
               (side . bottom)
               (slot . -1) ;; left side
               (preserve-size . (nil . t))
               ,nl/side-window-parameters))

(add-to-list 'display-buffer-alist
             '("\\*\\(?:[Hh]elp\\|Backtrace\\|Warnings\\|Completions\\|Compile-Log\\|\\*Flycheck.*\\|shell\\|compilation\\|ng-compile\\|ng-test\\|tide-references\\|sbt\\|coverlay-stats\\)\\*"
               display-buffer-in-side-window
               (window-height . 0.15)
               (side . bottom)
               (slot . 1) ;; right side
               (preserve-size . (nil . t))
               ,nl/side-window-parameters))

(setq-default indent-tabs-mode nil)

(setq bookmark-default-file "~/.config/.emacs.d/etc/bookmarks")

(windmove-default-keybindings 'meta)

(use-package hungry-delete
  :diminish hungry-delete-mode
  :init
  (global-hungry-delete-mode))

(use-package winner
  :demand t
  :config
  (winner-mode))

(use-package edit-server
  :if (display-graphic-p)
  :preface
  (defun nl/after-init-hook ()
    (server-start t)
    (edit-server-start t)
    (nl/set-fonts nil)
    )
  :init
  (add-hook 'after-init-hook 'nl/after-init-hook))

(use-package doom-themes
  :demand t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-acario-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom-face
  ;;(ansi-color-blue ((t (:foreground "#4f57f9"))))
  (ansi-color-blue ((t (:foreground "DeepSkyBlue1"))))
  ;;(lsp-face-highlight-read ((t (:foreground "DeepSkyBlue1"))))
  )

(with-eval-after-load 'markdown-mode
  ;;(set-face-foreground 'markdown-code-face (doom-darken 'green 0.2))
  ;;(set-face-background 'markdown-code-face (doom-color 'brightblack))
  ;;(set-face-background 'org-block (doom-color 'brightblack))
  (set-face-attribute 'markdown-header-face
                      nil
                      :font nl/gui-variable-font-name
                      :weight 'bold
                      :height (* 12 (string-to-number nl/gui-current-variable-font-size)))
  (set-face-attribute 'markdown-link-face
                      nil
                      :font nl/gui-variable-font-name
                      :weight 'bold
                      :height (* 10 (string-to-number nl/gui-current-variable-font-size)))
  )

(with-eval-after-load 'markdown-mode
  ;;(set-face-foreground 'markdown-code-face (doom-darken 'green 0.2))
  ;;(set-face-background 'markdown-code-face (doom-color 'brightblack))
  ;;(set-face-background 'org-block (doom-color 'brightblack))
  (set-face-attribute 'markdown-header-face
                      nil
                      :font nl/gui-variable-font-name
                      :weight 'bold
                      :height (* 12 (string-to-number nl/gui-current-variable-font-size)))
  (set-face-attribute 'markdown-link-face
                      nil
                      :font nl/gui-variable-font-name
                      :weight 'bold
                      :height (* 10 (string-to-number nl/gui-current-variable-font-size)))
  )

(defun ap/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (ap/switch-theme theme))

(defun ap/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

(use-package multiple-cursors
  :after selected
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-M->"       . mc/unmark-next-like-this)
         ("C-M-<"       . mc/unmark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-!"         . mc/mark-next-symbol-like-this)
         ("C-x C-m"     . mc/mark-all-dwim))
  :bind (:map selected-keymap
              ("C-'" . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this)))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map ("f" . consult-projectile))
  :init (projectile-mode +1)
  :config
  ;; tramp-fix: https://github.com/syl20bnr/spacemacs/issues/11381
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))

  (setq projectile-indexing-method 'alien
        projectile-remember-window-configs nil
        projectile-switch-project-action 'projectile-dired
        projectile-completion-system 'default
        projectile-enable-caching nil
        projectile-create-missing-test-files t
        projectile-mode-line "Projectile")

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired)))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-project)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; configure a function which returns the project root directory.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  )

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'

  :preface

  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))


  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Use the `orderless' completion style.
;; Enable `partial-completion' for file path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult-projectile
  :after (consult))

(use-package magit
  :bind (("C-x g" . magit-status))
  :hook (magit-mode . magit-todos-mode)
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (setq-default vc-handled-backends '(Git))
  (setq magit-push-always-verify nil

        ;; only use A and B in Ediff
        magit-ediff-dwim-show-on-hunks t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit-todos
  :diminish
  :after magit
  :custom
  (magit-todos-auto-group-items 'always)
  (magit-todos-group-by '(magit-todos-item-keyword magit-todos-item-filename))
  :config
  (magit-todos-mode))

(use-package git-timemachine
  :commands git-timemachine)

(defun nl/main-frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-position frame -1 0)
    (set-frame-size frame 229 (/ (x-display-pixel-height) (frame-char-height)))
    ))

(defun nl/frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (nl/frame-set-size-and-position-hook frame)
    )
  )

(defun nl/frame-set-size-and-position-hook (frame)
  (set-frame-position frame 2200 60)
  (set-frame-size frame 120 (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.80)))
  )

(add-hook 'after-make-frame-functions 'nl/frame-set-size-and-position-hook t)

;; (add-hook 'window-setup-hook (lambda ()
;;                                (nl/main-frame-set-size-and-position)
;;                                (make-frame-command)))

(use-package move-text
  :bind (("C-S-<up>" . move-text-up)
         ("C-S-<down>" . move-text-down)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (typescript-mode . yas-minor-mode)
  ;;:init
  ;;(yas-global-mode 1)
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

(use-package company
  :diminish company-mode
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("M-/" . company-complete-common))
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local company-backends '(company-elisp))))
   (emacs-lisp-mode . company-mode))
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-abort-manual-when-too-short t "Be less enthusiastic about completion.")
  :custom-face
  (company-tooltip ((t (:family "FiraCode Nerd Font" :height 100))))
  :config
  (setq company-idle-delay 0              ;; no delay no autocomplete
        company-minimum-prefix-length 1
        company-tooltip-limit 20)
  )

(use-package flycheck
  :commands global-flycheck-mode
  :diminish flycheck-mode
  :commands flycheck-define-checker
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-standard-error-navigation nil)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq flycheck-checkers (append flycheck-checkers
                                  '(javascript-eslint))
        flycheck-python-flake8-executable "flake8")
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode))

(use-package column-enforce-mode
  :config
  (setq column-enforce-column 120)
  :hook (progmode-hook . column-enforce-mode))2

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'")
  :hook
  (typescript-mode . lsp-deferred)
  (typescript-mode . column-enforce-mode)
  ;;(typescript-mode . rainbow-delimiters-mode)
  (typescript-mode . nl/typescript-mode)
  :preface
  (defun nl/typescript-mode ()
    (flycheck-mode +1)
    (eldoc-mode +1)
    (company-mode +1)

    ;; need to override the value set in typescript-mode.el
    (push '(typescript-tsc-pretty
            "^\\(?:\\(Error\\|Warning\\)\\):[[:blank:]]\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
            2 3 4 1)
          compilation-error-regexp-alist-alist)

    ;;(setq prettify-symbols-alist nl-typescript-prettify-symbols)
    (prettify-symbols-mode))
  :config
  (setq ;;prettify-symbols-unprettify-at-point 'right-edge
   company-tooltip-align-annotations t ;; aligns annotation to the right hand side
   flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default typescript-indent-level 4)

  ;;(defconst nl-typescript-prettify-symbols
  ;;  '(("=>" . ?⇒)
  ;;    ("<=" . ?≤)
  ;;    (">=" . ?≥)
  ;;    ("===" . ?≡)
  ;;    ("!" . ?¬)
  ;;    ("!=" . ?≠)
  ;;    ("!==" . ?≢)
  ;;    ("&&" . ?∧)
  ;;    ("||" . ?∨)))
  )


(use-package web-mode
  :hook ((web-mode . lsp)
         (typescript-tsx-mode . lsp))
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript-tsx")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package prettier
  :hook ((typescript-tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

(use-package lsp-mode
  ;;:load-path "~/src/github/elisp/lsp-mode"
  :pin melpa
  :commands (lsp lsp-deferred)
  :hook
  ;;(js-mode . lsp)
  (typescript-mode . lsp)
  (scala-mode . lsp)
  ;;(php-mode . lsp)
  ;; (python-mode . lsp) ;; commented out because lsp is initialized in lsp-pyright config
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet t)
  (lsp-enable-file-watchers nil)
  (lsp-pyls-plugins-pycodestyle-max-line-length 120)
  (lsp-intelephense-php-version "7.4.25")
  (lsp-intelephense-format-enable nil)
  ;;(setq lsp-response-timeout 25)
  :config
  (setq lsp-prefer-capf t
        lsp-idle-delay 0.5
        lsp-pyls-plugins-flake8-enabled t
        ;; lsp-serenata-server-path (substitute-in-file-name "$HOME/apps/serenata.phar")
        ;; lsp-serenata-index-database-uri (substitute-in-file-name "$HOME/.emacs.d/serenata-index.sqlite")
        ;; lsp--tcp-server-port 11111
        ;; lsp-enabled-clients '(serenata)
        ;; lsp-serenata-php-version 7.4
        lsp-ensabled-clients '(intelephense))
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/home/nelson/.nvm/versions/node/v16.13.2/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/home/nelson/.nvm/versions/node/v16.13.2/lib/node_modules"
          "--tsProbeLocations"
          "/home/nelson/.nvm/versions/node/v16.13.2/lib/node_modules"
          "--stdio"))
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))

(use-package lsp-ui
  ;; :load-path "~/src/github/elisp/lsp-ui"
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ([f10] . lsp-ui-sideline-toggle-symbols-info))
  :custom-face
  (lsp-ui-peek-peek ((nil :background "gray30")))
  (lsp-ui-peek-highlight ((nil :foreground "gray60" :background "gray20")))
  (header-line ((t (:inherit mode-line :background "gray30"))))
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (flycheck-add-next-checker 'lsp-ui 'typescript-tslint)
  :config
  (setq lsp-ui-peek-always-show nil
        lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  )

;;; Finalization

;; comment this line out to show the *Messages* buffer on startup
;; (nl/show-messages-on-startup)
