;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(use-package nl-php-project :demand :load-path "~/.emacs.d/lisp")
(require 'nl-php-project)

(defvar nl/norweb-project-root (projectile-project-root))

(projectile-register-project-type 'php-symfony '("composer.json" "src" "test" "vendor")
                                  :project-file "composer.json"
                                  :src-dir "src/"
				  :test "make test"
                                  :test-suffix "Test"
				  :test-dir "test/")

(defun nl/php-create-docker-command (command)
  "Create a COMMAND that can be run using the docker-wrapper.sh shell script."
  (format "cd %s && ./docker-wrapper.sh app-cmd \"%s\"" nl/norweb-project-root command))

(defun nl/php-docker-command-in-proj-root (command)
  "Run the COMMAND under the docker-wrapper.sh shell script."
  (interactive)
  (compile (nl/php-create-docker-command command)))

(defun nl/phpunit-run-this-method-with-debug-logging ()
  "Run PHPUnit with COMMAND in Norweb docker container."
  (interactive)
  (let ((command (format "LOG_LEVEL=DEBUG %s"
                         (nl/phpunit-create-command
                          (string-join (list
                                        "--filter "
                                        (nl/phpunit-test-find-method-name)
                                        " "
                                        (nl/phpunit-file-name)))))))
    ;;(message "command: %s" command)
    (nl/php-command-in-proj-root command)))

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
  ("d" nl/phpunit-run-this-method-with-debug-logging "only this method (with debug logging)")
  ("p" nl/phpunit-project "All tests" :column "Test")
  ("f" nl/phpunit-test-this-file "only this file")
  ("m" nl/phpunit-only-this-method "only this method")
  ("r" nl/phpunit-coverage-report-in-chrome "Open coverage report in Chrome"))

(defhydra hydra-nl-php-project (:color red :hint nil)
  "PHP project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "PHP")
  ("s" hydra-nl/php-search/body "search" :color blue)
  ("t" hydra-nl/php-test/body "test" :color blue)
  ("c" nl/php-code-sniffer "Run PHP CodeSniifer" :column "Make"))

(define-key php-mode-map (kbd "C-c , d") 'nl/phpunit-run-this-method-with-debug-logging)
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
