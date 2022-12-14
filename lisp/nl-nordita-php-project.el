;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(require 'nl-php-project)

(defvar nl/norweb-project-root (projectile-project-root))

(defconst php-beginning-of-class-regexp
  (rx line-start
      (* (syntax whitespace))
      (zero-or-more "final")
      (* (syntax whitespace))
      "class"
      (+ (syntax whitespace))
      (group (+ (or (syntax word) (syntax symbol))))
      (* (syntax whitespace)))
  "Regular expression for a PHP class name.")

(defconst phpunit-test-beginning-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        "public function"
        (one-or-more (syntax whitespace))
        (group "test" (+ (not (syntax open-parenthesis))))
        (one-or-more (not ":"))
        ":"
        ))
  "Regular expression for a PHPUnit test function.")

(defun nl/phpunit-test-find-class-name ()
  "Determine the name of the PHPUnit suite name."
  (save-excursion
    (when (re-search-backward php-beginning-of-class-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/phpunit-test-find-method-name ()
  "Determine the name of the PHPUnit test's method name."
  (save-excursion
    (when (re-search-backward phpunit-test-beginning-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/phpunit-file-name ()
  "Return the phpunit's file module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (unless (boundp 'nl/norweb-project-root) (error "The unknown norweb project"))
  (let ((file-name (nth 1 (split-string buffer-file-name nl/norweb-project-root))))
    (message "%s" (concat nl/norweb-project-root "/"))
    (unless file-name (error "File not in project"))
    (unless (nl/php-filename-p file-name) (error "not a PHP file"))
    file-name)
  )

(defun nl/php-command-in-proj-root (command)
  "Run the compile COMMAND in project's root directory."
  (interactive)
  (compile (format "cd %s && %s" nl/norweb-project-root command)))

(defun nl/php-docker-command-in-proj-root (command)
  "Run the compile COMMAND docker-wrapper.sh script at the project's root directory."
  (interactive)
  (compile (format "cd %s && ./docker-wrapper.sh app-cmd \"%s\"" nl/norweb-project-root command)))

(defun nl/phpunit-run (command)
  "Run PHPUnit with COMMAND in Norweb docker container."
  (nl/php-command-in-proj-root
   (format "vendor/bin/phpunit -c test/phpunit.xml --no-coverage --color=always %s" command)))

(defun nl/phpunit-test-this-file ()
  "For the class the cursor is in, run the scalatest test suite."
  (interactive)
  (nl/phpunit-run (nl/phpunit-file-name)))

(defun nl/phpunit-only-this-method ()
  "Run the PHPUnit test for the test the cursor is in."
  (interactive)
  (nl/phpunit-run
   (string-join (list "--filter "
                      (nl/phpunit-test-find-method-name)
                      " "
                      (nl/phpunit-file-name)))
   ))

(defun nl/phpunit-project ()
  "Run the PHPUnit test suite."
  (interactive)
  (nl/phpunit-run ""))

(defun nl/phpunit-coverage-report-in-chrome ()
  "Open the code coverage report in a Google Chrome tab."
  (interactive)
  (browse-url-chrome (format "file://%stest/coverage/report/index.html" (projectile-project-root))))

(defun nl/php-code-sniffer ()
  "Run PHP CodeSniffer through the project's Makefile."
  (interactive)
  (nl/php-command-in-proj-root "make php-sniff"))

(defhydra hydra-nl/php-test (:color blue)
  "Test"
  ("p" nl/phpunit-project "All tests" :column "Test")
  ("f" nl/phpunit-test-this-file "only this file")
  ("m" nl/phpunit-only-this-method "only this file")
  ("r" nl/phpunit-coverage-report-in-chrome "Open coverage report in Chrome"))

(defhydra hydra-nl-php-project (:color red :hint nil)
  "PHP project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "PHP")
  ("s" hydra-nl/php-search/body "search" :color blue)
  ("t" hydra-nl/php-test/body "test" :color blue)
  ("c" nl/php-code-sniffer "Run PHP CodeSniifer" :column "Make"))

(define-key php-mode-map (kbd "C-c , m") 'nl/phpunit-only-this-method)
(define-key php-mode-map (kbd "C-c , f") 'nl/phpunit-test-this-file)
(define-key php-mode-map (kbd "C-c , p") 'nl/phpunit-project)

(defun nl/projectile-test-suffix-function (project-type)
  "Return the suffix for test files for PROJECT-TYPE."
  (message "%s" project-type)
  (cond
   ((string-equal "php-symfony" project-type) "Test")))

(setq projectile-test-suffix-function 'nl/projectile-test-suffix-function
      projectile-find-dir-includes-top-level t)

;; (projectile-update-project-type
;;  'php
;;  :related-files-fn
;;  (list
;;   (projectile-related-files-fn-test-with-suffix "php" ".spec")
;;   (projectile-related-files-fn-test-with-suffix "php" "Test")))

;;
;; override this function, from the projectile package, so that tests are created in the proper
;; location for this project
;;
(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for the file given by IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (test-file-extension (file-name-extension impl-file-path))
         (test-dir))
    (cond
     ((string= test-file-extension "ts")
      (setq test-dir (file-name-directory impl-file-path)))
     ((string= test-file-extension "php")
      (setq test-dir (replace-regexp-in-string "src/" "test/" (file-name-directory impl-file-path)))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

(provide 'nl-nordita-php-project)
;;; nl-nordita-php-project.el ends here
