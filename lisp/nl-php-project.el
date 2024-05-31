;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(require 's)
(require 'php-mode)
(eval-and-compile
  (require 'yaml-mode)
  (require 'yaml-ts-mode)
  (require 'projectile))

(defun nl/php-filename-p (filename)
  "Return TRUE if the FILENAME ends in '.php''."
  (string-match-p "\\.php$" filename))

(setq-default indent-tabs-mode nil)

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

(defconst php-function-name-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        "public function"
        (one-or-more (syntax whitespace))
        (group (+ (not (syntax open-parenthesis))))
        ))
  "Regular expression for a PHP public function.")

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

(defconst phpunit-test-attribute-beginning-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        "#\[Test\]"
        ))
  "Regular expression for a PHPUnit test function that uses a PHP attribute.")

(defun nl/phpunit-file-name ()
  "Return the phpunit's file module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (let ((file-name (nth 1 (split-string buffer-file-name (project-root (project-current))))))
    (unless file-name (error "File not in project"))
    (unless (nl/php-filename-p file-name) (error "not a PHP file"))
    file-name)
  )

(defun nl/phpunit-test-find-class-name ()
  "Return the name of the PHPUnit suite name."
  (save-excursion
    (when (re-search-backward php-beginning-of-class-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/phpunit-test-find-method-name ()
  "Return the name of the PHPUnit test's method. The test method can start with 'test' or can use a PHP attribute."
  (save-excursion
    (if (re-search-backward phpunit-test-beginning-regexp nil t)
      (match-string-no-properties 1)
    (if (and (re-search-backward phpunit-test-attribute-beginning-regexp)
               (re-search-forward php-function-name-regexp))
      (match-string-no-properties 1))
    )))

(defun nl/php-file-name ()
  "Return the module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (let ((file-name (nth 1 (split-string buffer-file-name (project-root (project-current))))))
    (unless file-name (error "File not in project"))
    (unless (nl/php-filename-p file-name) (error "not a PHP file"))
    file-name)
  )

(defun nl/phpunit-create-command (command)
  "Create a COMMAND that can run a test using PHPUnit."
  (format "vendor/bin/phpunit -c test/phpunit.xml --testdox --no-coverage --exclude-group=end-to-end %s" command))

(defun nl/php-command-in-proj-root (command)
  "Run the compile COMMAND in project's root directory."
  (interactive)
  (compile (nl/php-create-command-in-project-root command)))

(defun nl/php-create-command-in-project-root (command)
  "Create a compile COMMAND that can be run from project's root directory."
  (format "cd %s && %s" (project-root (project-current)) command))

(defun nl/phpunit-run (command)
  "Run PHPUnit with COMMAND in Norweb docker container."
  (nl/php-command-in-proj-root
   (nl/phpunit-create-command command)))

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
                      (nl/php-file-name)))
   ))

(defun nl/phpunit-project ()
  "Run the PHPUnit test suite."
  (interactive)
  (nl/phpunit-run ""))

(defun nl/phpunit-test-this-package ()
  "For the class the cursor is in, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  (let ((args (s-concat (file-name-directory (buffer-file-name)))))
    (phpunit-run args)))

;; -----------------------------------------
;;
;; -----------------------------------------

(defhydra hydra-nl-project (:color red :hint nil)
  "Project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "PHP"))

(key-chord-define php-mode-map "jc" 'hydra-nl-project/body)
(key-chord-define yaml-ts-mode-map "jc" 'hydra-nl-project/body)

(defun nl/phpunit-use-attributes ()
  (interactive)
  (when (re-search-forward "^\\s-+public function\\s-+test" nil t)
    (backward-kill-word 1)
    (downcase-word 1)
    (back-to-indentation)
    (insert "#[Test]\n")
    (c-indent-line-or-region)
  ))

(provide 'nl-php-project)
;;; nl-php-project.el ends here
