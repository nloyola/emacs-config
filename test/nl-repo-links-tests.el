;;; nl-repo-links-tests.el --- Tests for per-repository links -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from the repository root:
;;
;;   emacs -Q --batch -L lisp -l test/nl-repo-links-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'nl-repo-links)

(ert-deftest nl-repo-slug-from-ssh-and-https-remotes ()
  (should (equal (nl/repo-github-slug "git@github.com:nordita-stockholm/norweb-2021.git")
                 "nordita-stockholm/norweb-2021"))
  (should (equal (nl/repo-github-slug "https://github.com/openta-development/openta-private")
                 "openta-development/openta-private"))
  (should (equal (nl/repo-github-slug "https://github.com/openta-development/openta-private.git/")
                 "openta-development/openta-private")))

(ert-deftest nl-repo-slug-is-nil-off-github ()
  (should-not (nl/repo-github-slug "git@gitlab.com:someone/thing.git"))
  (should-not (nl/repo-github-slug nil)))

(ert-deftest nl-repo-norweb-issues-url-keeps-its-filter ()
  (should (equal (nl/repo-issues-url (assoc "nordita-stockholm/norweb-2021" nl/repo-links))
                 (concat "https://github.com/nordita-stockholm/norweb-2021/issues?q="
                         "is%3Aissue%20state%3Aopen%20-label%3Acontent%20-label%3ASymfony"
                         "%20-label%3Adb-refactor%20-label%3A%22Build%20Tools%22"
                         "%20-label%3AInfrastructure"))))

(ert-deftest nl-repo-issues-url-defaults-to-open-issues ()
  (should (equal (nl/repo-issues-url (assoc "openta-development/openta-private" nl/repo-links))
                 "https://github.com/openta-development/openta-private/issues?q=is%3Aissue%20state%3Aopen")))

(ert-deftest nl-repo-current-reads-the-origin-remote ()
  (let* ((dir (make-temp-file "nl-repo-links" t))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (call-process "git" nil nil nil "init" "-q")
          (call-process "git" nil nil nil "remote" "add" "origin"
                        "git@github.com:openta-development/openta-private.git")
          (should (equal (plist-get (cdr (nl/repo-current)) :dropbox) "~/Dropbox/openTA/")))
      (delete-directory dir t))))

(provide 'nl-repo-links-tests)
;;; nl-repo-links-tests.el ends here
