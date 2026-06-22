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

;; Ensure encrypted files still decrypt while startup temporarily trims
;; `file-name-handler-alist'.  Some files can be visited before startup
;; finishes (for example via command-line arguments or desktop restore).
(require 'epa-file)
(epa-file-enable)

(defvar nl/file-name-handler-alist-original file-name-handler-alist)

;; from https://github.com/D4lj337/Emacs-performance
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;; Keep `file-name-handler-alist' intact.  Disabling it speeds startup, but
;; breaks encrypted Org files when they are visited before startup finishes.

(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil
      frame-inhibit-implied-resize t
      pixel-scroll-precision-mode t
      byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

(defun nl/after-init ()
  (setq file-name-handler-alist nl/file-name-handler-alist-original
        gc-cons-threshold 200000000
        gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'after-init-hook #'nl/after-init)

(setq comp-deferred-compilation t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; when tramp is slow
;;
;; - not working really
;;(setq projectile-mode-line "Projectile")

;;; Bootstrap Elpaca and use-package
(setq package-enable-at-startup nil)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil
           :depth 1
           :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca-activate)))
(let* ((repo (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                   ,@(when-let* ((depth (plist-get order :depth)))
                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
                                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn
              (message "%s" (buffer-string))
              (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error)
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(setq-default use-package-compute-statistics nil  ; to check if config is ok
              use-package-always-ensure t         ; Auto-download package if not exists
              use-package-always-defer t          ; Always defer load package to speed up startup time
              use-package-expand-minimally nil    ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; use only for debugging startup time
;;(setq use-package-verbose nil)
(setq use-package-verbose t)               ; report loading details

;; `use-package` and its helpers need to be available before any
;; `use-package` forms are expanded or loaded.
(use-package diminish :demand t)
(use-package bind-key :ensure nil :demand t)
(use-package s :demand t)
(use-package htmlize :defer t)
(use-package key-chord
  :demand t
  :commands key-chord-define-global
  :config
  (key-chord-mode 1))
(use-package use-package-chords
  :demand t
  :after key-chord
  :config (key-chord-mode 1))
(elpaca-wait)

(use-package pl
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :commands pl-parse)

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
