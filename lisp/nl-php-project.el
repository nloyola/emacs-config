;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(require 's)
(require 'php-mode)
(eval-and-compile
  (require 'yaml-mode)
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

(defun nl/phpunit-file-name ()
  "Return the phpunit's file module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (let ((file-name (nth 1 (split-string buffer-file-name nl/norweb-project-root))))
    (message "%s" (concat nl/norweb-project-root "/"))
    (unless file-name (error "File not in project"))
    (unless (nl/php-filename-p file-name) (error "not a PHP file"))
    file-name)
  )

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

(defun nl/php-file-name ()
  "Return the module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (let ((file-name (nth 1 (split-string buffer-file-name nl/norweb-project-root))))
    (message "%s" (concat (projectile-project-root) "/"))
    (unless file-name (error "File not in project"))
    (unless (nl/php-filename-p file-name) (error "not a PHP file"))
    file-name)
  )

(defun nl/phpunit-create-command (command)
  "Create a COMMAND that can run a test using PHPUnit."
  (format "vendor/bin/phpunit -c test/phpunit.xml --no-coverage --exclude-group=end-to-end %s" command))

(defun nl/php-command-in-proj-root (command)
  "Run the compile COMMAND in project's root directory."
  (interactive)
  (compile (nl/php-create-command-in-project-root command)))

(defun nl/php-create-command-in-project-root (command)
  "Create a compile COMMAND that can be run from project's root directory."
  (format "cd %s && %s" nl/norweb-project-root command))

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

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define php-mode-map "jc" '(lambda () (interactive) (hydra-nl-project/body)))
(key-chord-define yaml-mode-map "jc" '(lambda () (interactive) (hydra-nl-project/body)))

(provide 'nl-php-project)
;;; nl-php-project.el ends here
