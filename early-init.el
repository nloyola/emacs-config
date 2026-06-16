;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings that must be applied before `init.el' is loaded.

;;; Code:

;; Elpaca manages packages, so prevent Emacs from loading package.el first.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
