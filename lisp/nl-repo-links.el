;;; nl-repo-links.el --- Per-repository web and folder links -*- lexical-binding: t; -*-

;;; Commentary:

;; One table of the repositories I work on, keyed by the GitHub slug of
;; their `origin' remote, and commands that open the current repo's
;; GitHub page, its issue list and its Dropbox folder.  Matching on the
;; remote rather than the directory name keeps clones and worktrees
;; working wherever they live.

;;; Code:

(require 'subr-x)
(require 'url-util)

(defvar nl/repo-links
  '(("nordita-stockholm/norweb-2021"
     :issues-query "is:issue state:open -label:content -label:Symfony -label:db-refactor -label:\"Build Tools\" -label:Infrastructure"
     :dropbox "~/Dropbox/nordita/"
     :hydra hydra-nl-nordita-project/body
     :feature nl-nordita-php-project)
    ("openta-development/openta-private"
     :dropbox "~/Dropbox/openTA/"))
  "Repositories with links, as (SLUG . PLIST).
SLUG is the GitHub owner/name of the `origin' remote.  PLIST keys:
`:issues-query' filters the issue list (default: open issues),
`:dropbox' is the related local folder, `:hydra' is the repo's own
project hydra and `:feature' the library that defines it.")

(defun nl/repo-github-slug (url)
  "Return the GitHub owner/name in remote URL, or nil if it is not GitHub."
  (when (and url (string-match "github\\.com[:/]\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?/?\\'" url))
    (match-string 1 url)))

(defun nl/repo-origin-url ()
  "Return the `origin' remote URL of the repo around `default-directory'."
  (with-temp-buffer
    (when (zerop (process-file "git" nil t nil "config" "--get" "remote.origin.url"))
      (string-trim (buffer-string)))))

(defun nl/repo-current ()
  "Return (SLUG . PLIST) for the current repo, or nil when it has no entry."
  (when-let* ((slug (nl/repo-github-slug (nl/repo-origin-url))))
    (or (assoc slug nl/repo-links) (list slug))))

(defun nl/repo--require ()
  "Return the current repo entry, or signal a `user-error'."
  (or (nl/repo-current) (user-error "Not in a GitHub repository")))

(defun nl/repo-github-url (entry)
  "Return the GitHub page of repo ENTRY."
  (concat "https://github.com/" (car entry)))

(defun nl/repo-issues-url (entry)
  "Return the issue list URL of repo ENTRY, filtered by its `:issues-query'."
  (concat (nl/repo-github-url entry) "/issues?q="
          (url-hexify-string (or (plist-get (cdr entry) :issues-query)
                                 "is:issue state:open"))))

(defun nl/repo-open-github ()
  "Open the current repo's GitHub page in the browser."
  (interactive)
  (browse-url (nl/repo-github-url (nl/repo--require))))

(defun nl/repo-open-issues ()
  "Open the current repo's issue list in the browser."
  (interactive)
  (browse-url (nl/repo-issues-url (nl/repo--require))))

(defun nl/repo-open-dropbox ()
  "Open the current repo's Dropbox folder in Chrome as a local file listing."
  (interactive)
  (let ((dir (plist-get (cdr (nl/repo--require)) :dropbox)))
    (unless dir (user-error "No Dropbox folder for this repository"))
    (browse-url-chrome (concat "file://" (expand-file-name dir)))))

(with-eval-after-load 'hydra
  (defhydra hydra-nl-repo (:color blue :hint nil)
    "Repository"
    ("p" nl/repo-open-github "Open project page" :column "GitHub")
    ("i" nl/repo-open-issues "Open issues")
    ("d" nl/repo-open-dropbox "Open Dropbox folder" :column "Dropbox")
    ("g" hydra-nl-common/body "common" :column "Common")))

(defun nl/repo-hydra ()
  "Open the current repo's project hydra.
Use the repo's own hydra when it has one, the repository hydra when
the repo is in `nl/repo-links', and the common hydra otherwise."
  (interactive)
  (let* ((entry (nl/repo-current))
         (plist (cdr entry)))
    (cond
     ((plist-get plist :hydra)
      (require (plist-get plist :feature))
      (funcall (plist-get plist :hydra)))
     ((assoc (car entry) nl/repo-links)
      (hydra-nl-repo/body))
     (t (hydra-nl-common/body)))))

(provide 'nl-repo-links)
;;; nl-repo-links.el ends here
