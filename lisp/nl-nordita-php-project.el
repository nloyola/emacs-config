;;; -*- lexical-binding: t; -*-
;;;
;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(use-package nl-php-project :demand :load-path ("~/.emacs.d/lisp" "~/.emacs.d/elpa"))
(require 'nl-php-project)

(projectile-register-project-type 'php-symfony '("composer.json" "src" "test" "vendor")
                                  :project-file "composer.json"
                                  :src-dir "src/"
				  :test "make test"
                                  :test-suffix "Test"
				  :test-dir "test/")

(defun nl/php-create-docker-command (command)
  "Create a COMMAND that can be run using the docker-wrapper.sh shell script."
  (format "cd %s && ./docker-wrapper.sh app-cmd \"%s\"" (project-root (project-current)) command))

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

(defun nl/phpunit-run-this-method-with-x-debug ()
  "Run PHPUnit with COMMAND in Norweb docker container."
  (interactive)
  (let ((command (format "LOG_LEVEL=DEBUG XDEBUG_CONFIG=\"start_with_request=1\" %s"
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
  (browse-url-chrome (format "file://%stest/coverage/report/index.html" (project-root (project-current)))))

(defun nl/php-code-sniffer ()
  "Run PHP CodeSniffer through the project's Makefile."
  (interactive)
  (nl/php-command-in-proj-root "make php-sniff"))

(defun nl/nordita-symbol-scss-grep ()
  "Find text in the project's SCSS subfolder."
  (interactive)
  (unless (buffer-file-name) (user-error "not a file buffer"))
  (let* ((default-directory (format "%s/pw-frontend/src/scss" (project-root (project-current)))))
    (compile (format "grep --exclude-dir=_attic -nre \"%s\" ." (symbol-at-point)))))

(defun nl/nordita-symbol-pw-grep ()
  "Find text in the project's source code."
  (interactive)
  (let* ((default-directory (format "%s" (project-root (project-current)))))
    (compile (format "grep -nre \"%s\" {default_pages,src,pw-frontend/src/ts}" (symbol-at-point)))))

(defhydra hydra-nl/php-test (:color blue)
  "Test"
  ("d" nl/phpunit-run-this-method-with-debug-logging "only this method (with debug logging)" :column "PHP")
  ("x" nl/phpunit-run-this-method-with-x-debug "only this method (with x_debug)" :column "PHP")
  ("p" nl/phpunit-project "All tests")
  ("f" nl/phpunit-test-this-file "only this file")
  ("m" nl/phpunit-only-this-method "only this method")
  ("r" nl/phpunit-coverage-report-in-chrome "Open coverage report in Chrome")
  ("t" nl/phpunit-end-to-end-toggle "toggle exclude end-to-end" :column "End to End"))

(defhydra hydra-nl-nordita-project (:color red :hint nil)
  "Project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "General")
  ("t" hydra-nl/php-test/body "test" :color blue :column "PHP")
  ("c" nl/php-code-sniffer "Run PHP CodeSniffer" :column "Build")
  ("y" nl/nordita-build-page-from-yaml "Build ProcessWire page from YAML file" :color blue)
  ("g" hydra-nl-common/body "common" :color blue :column "Common"))

(define-key php-ts-mode-map (kbd "C-c , d") 'nl/phpunit-run-this-method-with-debug-logging)
(define-key php-ts-mode-map (kbd "C-c , x") 'nl/phpunit-run-this-method-with-x-debug)
(define-key php-ts-mode-map (kbd "C-c , m") 'nl/phpunit-only-this-method)
(define-key php-ts-mode-map (kbd "C-c , f") 'nl/phpunit-test-this-file)
(define-key php-ts-mode-map (kbd "C-c , p") 'nl/phpunit-project)
(define-key php-ts-mode-map (kbd "C-c , s") 'nl/phpunit-selenium-only-this-method)

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

;;(require 'casual-suite)

;; (transient-define-prefix casual-nl-align-tmenu ()
;;   "Transient menu for aligning things."
;;   [["Align things"
;;     (":" "colon" align-colons :transient nil)
;;     ("," "comma" align-commas :transient nil)
;;     ("=" "equals" (lambda () (interactive) (align-equals)) :transient nil)
;;     ("$" "dollar sign" (lambda () (interactive) (align-dollar-sign)) :transient nil)
;;     ("p" "parameters" align-parameters :transient nil)
;;    ]]
;;   )

;; (transient-define-prefix casual-nl-norweb-project-tmenu ()
;;   "Transient menu for Norweb 2021 Project."
;;   [["Align"
;;     ("a" "Align things" casual-nl-align-tmenu :transient nil)]]
;;   [["PHP Code Sniffer"
;;     ("c" "Run CodeSniffer" nl/php-code-sniffer :transient nil)
;;     ("s" "Run PHPStan" (lambda () (interactive)(nl/php-command-in-proj-root "composer phpstan")) :transient nil)
;;     ]]
;;   [["PHP Test"
;;     ("d" "only this method (with debug logging)" nl/phpunit-run-this-method-with-debug-logging :transient nil)
;;     ("p" "All tests" nl/phpunit-project :transient nil)
;;     ("f" "only this file" nl/phpunit-test-this-file :transient nil)
;;     ("m" "only this method" nl/phpunit-only-this-method :transient nil)
;;     ("r" "Open coverage report in Chrome" nl/phpunit-coverage-report-in-chrome :transient nil)
;;     ("t" "toggle exclude end-to-end" nl/phpunit-end-to-end-toggle :transient nil)
;;     ]]
;;   [["Build"
;;     ("o" "Open Other" dired-find-file-other-window :transient nil)]]
;;   [["YAML"
;;     ("y" "build PW YAML file" nl/nordita-build-page-from-yaml :transient nil)]]
;;   )

(key-chord-define php-ts-mode-map "jc" 'hydra-nl-nordita-project/body)

;;(key-chord-define php-ts-mode-map "jc" 'casual-nl-norweb-project-tmenu)


(provide 'nl-nordita-php-project)
;;; nl-nordita-php-project.el ends here
