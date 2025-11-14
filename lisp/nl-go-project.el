;;; -*- lexical-binding: t; -*-
;;;
;;; nl-go-project --- helpers for Go development
;;;
;;; Commentary:
;;  Custom Projectile configuration and Hydras for Go projects.
;;  Works with Projectile 3.x and newer.
;;;
;;; Code:

(require 's)
(require 'go-ts-mode)
(eval-and-compile
  (require 'projectile))

;; ----------------------------------------------------------------------
;; Projectile setup
;; ----------------------------------------------------------------------

;; Remove built-in Go type so ours takes precedence
(setq projectile-project-types
      (assq-delete-all 'go projectile-project-types))
(setq projectile-project-types
      (assq-delete-all 'gocustom projectile-project-types))

(defun nl/related-files (file)
  "Tell Projectile how to recognise related Go test and impl files."
  (if (string-match-p "_test\\.go\\'" file)
      ;; We are in a test file → describe how to find its implementation
      (list :test
            (lambda (other-file)
              (string= (file-truename other-file)
                       (file-truename
                        (s-replace "_test.go" ".go" file)))))
    ;; We are in an implementation file → describe how to find its test
    (list :impl
          (lambda (other-file)
            (string= (file-truename other-file)
                     (file-truename
                      (s-replace ".go" "_test.go" file)))))))

;; Register custom Go project type
(projectile-register-project-type 'gocustom '("Makefile" "go.mod")
                                  :project-file "go.mod"
                                  :src-dir ""
                                  :test "make test"
                                  :test-prefix ""
                                  :test-suffix "_test"
                                  :test-dir ""
                                  :related-files-fn #'nl/related-files)

;; Ensure Projectile treats top-level dirs as searchable
(setq projectile-find-dir-includes-top-level t)

;; ----------------------------------------------------------------------
;; Test runners
;; ----------------------------------------------------------------------

(defun nl/go-command-in-proj-root (command)
  "Run the compile COMMAND in project's root directory."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile command)))

;; ----------------------------------------------------------------------
;; Helpers
;; ----------------------------------------------------------------------

;; go test ./internal/domain -run TestServiceTypeServiceListAll -v

(defconst go-test-function-name-regexp
  (eval-when-compile
    (rx line-start
        (zero-or-more (syntax whitespace))
        "func"
        (one-or-more (syntax whitespace))
        (group "Test" (+ (not (any "(" whitespace)))))
        )
  "Regular expression matching a Go test function definition line.")

(defun nl/go-test-find-function-name ()
  "Return the name of the nearest Go test function (starting with 'Test')."
  (save-excursion
    (when (re-search-backward go-test-function-name-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/go-test-function-run ()
  "Run Go test for the current test function only, limited to the file's directory."
  (interactive)
  (let* ((root (projectile-project-root))
         (relpath (file-relative-name (file-name-directory (buffer-file-name)) root))
         (testname (nl/go-test-find-function-name)))
    (nl/go-command-in-proj-root
     (format "go test -count=1 ./%s -run ^%s$ -v" relpath testname))))

(defun nl/go-test-package-run ()
  "Run Go test for the package the file is in."
  (interactive)
  (let* ((root (projectile-project-root))
         (relpath (file-relative-name (file-name-directory (buffer-file-name)) root)))
    (nl/go-command-in-proj-root
     (format "go test -count=1 ./%s -v" relpath))))

(defun nl/go-fmt ()
  "Run Go code formater."
  (interactive)
  (nl/go-command-in-proj-root
   (format "make fmt")))

(defun nl/go-tidy ()
  "Clean up and sync go.mod/go.sum"
  (interactive)
  (nl/go-command-in-proj-root
   (format "make tidy")))

(defun nl/go-air ()
  "Clean up and sync go.mod/go.sum"
  (interactive)
  (nl/go-command-in-proj-root
   (format "air")))

(defun nl/go-build ()
  "Runs make build"
  (interactive)
  (nl/go-command-in-proj-root
   (format "make build")))

(defun nl/go-test ()
  "Runs make test"
  (interactive)
  (nl/go-command-in-proj-root
   (format "make test")))

(defun nl/go-test-coverage ()
  "Runs tests with coverage analysis"
  (interactive)
  (nl/go-command-in-proj-root
   (format "go tool cover -html=coverage.out -o coverage.html")))

(defun nl/go-test-coverage-report-in-chrome ()
  "Open the Go code coverage report in Google Chrome.
Raise an error if coverage.html does not exist in the project root."
  (interactive)
  (let* ((proj-root (project-root (project-current)))
         (coverage-file (expand-file-name "coverage.html" proj-root)))
    (if (file-exists-p coverage-file)
        (browse-url-chrome (concat "file://" coverage-file))
      (user-error "Coverage report not found: %s" coverage-file))))

;; ----------------------------------------------------------------------
;; Hydras
;; ----------------------------------------------------------------------

(defhydra hydra-nl/go-test (:color blue)
  "Test"
  ("c" nl/go-test-coverage "run test suite with coverage analysis" :column "Go")
  ("C" nl/go-test-coverage-report-in-chrome "open coverage report in chrome" :column "Go")
  ("f" nl/go-test-function-run "only this method (with debug logging)" :column "Go")
  ("m" nl/go-test "make test")
  ("p" nl/go-test-package-run "All tests for package")
  )

(defhydra hydra-nl-go-project (:color red :hint nil)
  "Project commands"
  ("t" hydra-nl/go-test/body "test" :color blue :column "Go")
  ("a" nl/go-air "Run Air" :column "Build" :color blue)
  ("b" nl/go-build "Build" :column "Build" :color blue)
  ("f" nl/go-fmt "Format Go code" :column "Build" :color blue)
  ("T" nl/go-tidy "Tidy Go code" :column "Build" :color blue)
  ("g" hydra-nl-common/body "common" :color blue :column "Common"))

;; Optional chord to quickly open project hydra
(key-chord-define go-ts-mode-map "jc" #'hydra-nl-go-project/body)

;; ----------------------------------------------------------------------
;; Key bindings
;; ----------------------------------------------------------------------

(define-key go-ts-mode-map (kbd "C-c , t") 'go-test-current-test)
(define-key go-ts-mode-map (kbd "C-c , f") 'go-test-current-file)
(define-key go-ts-mode-map (kbd "C-c , p") 'go-test-current-project)

;; ----------------------------------------------------------------------
(provide 'nl-go-project)
;;; nl-go-project.el ends here
