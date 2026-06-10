;;; -*- lexical-binding: t; -*-

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

(defvar nl/file-name-handler-alist-original file-name-handler-alist)

;; from https://github.com/D4lj337/Emacs-performance
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;; Disable file-name-handler-alist during startup for speed; restore after init.
(setq file-name-handler-alist nil)

(setq package-install-upgrade-built-in t
      package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil
      frame-inhibit-implied-resize t
      pixel-scroll-precision-mode t)

(defun nl/after-init ()
  (setq file-name-handler-alist nl/file-name-handler-alist-original
        gc-cons-threshold 200000000
        gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'emacs-startup-hook #'nl/after-init)

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

(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))

(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "http://elpa.nongnu.org/packages/") t))

(setf (alist-get "gnu" package-archives nil nil #'string=)
      "https://www.mirrorservice.org/sites/elpa.gnu.org/packages/")

(setq package-archive-priorities
      '(("gnu"    . 99)
        ("nongnu" . 80)
        ("melpa"  . 0)))

(unless package--initialized
  (package-initialize))

;; Using HTTPS for downloading packages, make sure HTTPS is not going through a proxy.
;; (setenv "https_proxy" "")
;; (setenv "http_proxy" "")

;;; Bootstrap use-package
(setq-default use-package-compute-statistics nil  ; to check if config is ok
              use-package-always-ensure t         ; Auto-download package if not exists
              use-package-always-defer t          ; Always defer load package to speed up startup time
              use-package-expand-minimally nil    ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; use only for debugging startup time
(setq use-package-verbose t)               ; report loading details)

(defun nl/ensure-package-installed (package)
  "Install PACKAGE unless it is already installed."
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

;; `use-package` and its helpers need to be available before any
;; `use-package` forms are expanded or loaded.
(dolist (package '(use-package diminish bind-key htmlize key-chord use-package-chords))
  (nl/ensure-package-installed package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)
;;(setq use-package-verbose nil)
(setq use-package-verbose t)

(use-package pl
  :load-path "~/.emacs.d/lisp"
  :commands pl-parse
  )

(use-package key-chord
  :demand t
  :commands key-chord-define-global
  :config
  (key-chord-mode 1))

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

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



(defun nl/eval-config-org-file (path errors)
  "Evaluate emacs-lisp src blocks in PATH, pushing failures onto ERRORS.
Returns the updated ERRORS list. Blocks tagged `:tangle no' are skipped.
Errors are accumulated rather than raised, matching the original behavior
documented at http://emacsninja.com/posts/failing-gracefully.html"
  (with-temp-buffer
    ;; Pad with a leading newline so the loop's initial `forward-line 1'
    ;; lands on the file's real line 1. Without this, line 1 — which in
    ;; split files is typically the top-level heading — gets skipped.
    (insert "\n")
    (insert-file-contents path)
    (goto-char (point-min))
    (let (heading section-decl src-beg src-end)
      (while (not (eobp))
	(forward-line 1)
	(pl-parse
	 (pl-re "^\\*\\{1,5\\} +.*$" :beg)
	 (setq heading (match-string 0)))
	(pl-parse
	 (pl-re "^#\\+BEGIN_SRC +emacs-lisp.*$" :beg)
	 (setq src-beg (match-end 0))
	 (setq section-decl (match-string 0))
	 (pl-until
	  (pl-re "\n#\\+END_SRC$" :end))
	 (setq src-end (match-beginning 0))
	 (unless (string-match ":tangle +no" section-decl)
	   (condition-case error
	       (progn
		 (message "%s" heading)
		 (with-timer (eval-region src-beg src-end)))
	     (error
	      (push (format "[%s] %s for:\n%s\n\n---\n"
			    (file-name-nondirectory path)
			    (error-message-string error)
			    (buffer-substring src-beg src-end))
		    errors))))))))
  errors)

(defun load-config-org ()
  "Evaluate every `config/*.org' file in lexicographic order.
Errors are accumulated and reported in the *init errors* buffer."
  (let ((errors '())
	(dir (expand-file-name "config" user-emacs-directory)))
    (dolist (file (directory-files dir t "\\.org\\'"))
      (setq errors (nl/eval-config-org-file file errors)))
    (when errors
      (with-current-buffer (get-buffer-create "*init errors*")
	(insert (format "%i error(s) found\n\n" (length errors)))
	(dolist (err (nreverse errors))
	  (insert err "\n"))
	(goto-char (point-min))
	(special-mode))
      (setq initial-buffer-choice (lambda () (get-buffer "*init errors*"))))))

(load-config-org)

;;; Finalization

;; comment this line out to show the *Messages* buffer on startup
;; (defun nl/show-messages-on-startup ()
;;   "Show the *Messages* buffer after starting Emacs."
;;   (setq initial-buffer-choice (lambda () (get-buffer "*Messages*"))))

;; (nl/show-messages-on-startup)
