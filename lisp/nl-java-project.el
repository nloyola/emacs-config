;;; nl-java-project --- package to help with development of the java projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:


(eval-and-compile
  (require 'projectile)
  (require 'java-ts-mode))

(defconst java-beginning-of-class-regexp
  (rx line-start
      (* (syntax whitespace))
      (zero-or-more "final")
      (* (syntax whitespace))
      "class"
      (+ (syntax whitespace))
      (group (+ (or (syntax word) (syntax symbol))))
      (* (syntax whitespace)))
  "Regular expression for a Java class name.")

(defconst junit-test-beginning-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        (| "@Test" "@ParameterizedTest")))
  "Regular expression for a JUnit test function that uses an annotation.")

(defconst java-function-name-regexp
  (eval-when-compile
    (rx line-start
        (zero-or-more blank)
        (zero-or-more "public")
        (one-or-more blank)
        (zero-or-more (or alnum "_"))
        (one-or-more blank)
        (group (one-or-more (or alnum "_")))
        (syntax open-parenthesis)
        )
    )
  "Regular expression for a public function.")

(defun nl/java-filename-p (filename)
  "Return TRUE if the FILENAME ends in '.java''."
  (string-match-p "\\.java$" filename))

(defun nl/java-create-command-in-project-root (command)
  "Create a compile COMMAND that can be run from project's root directory."
  (format "cd %s && %s" (projectile-project-root) command))

(defun nl/java-junit-create-command (command)
  "Create a COMMAND that can run a test using JUnit."
  ;; mvn test -Dtest="AuthControllerTest#tokenWithBasicThenGetToken"
  (format "mvn %s test" command))

(defun nl/java-command-in-proj-root (command)
  "Run the compile COMMAND in project's root directory."
  (interactive)
  (compile (nl/java-create-command-in-project-root command)))

(defun nl/java-junit-html ()
  "Creates the JUnit HTML report."
  (interactive)
  (compile (nl/java-create-command-in-project-root "mvn clean verify surefire-report:report"))
  (nl/java-test-report-in-chrome))

(defun nl/java-junit-run (command)
  "Run JUnit with COMMAND in Norweb docker container."
  (nl/java-command-in-proj-root
   (nl/java-junit-create-command (format "-Dtest=\"%s\"" command))))

(defun nl/java-junit-run-with-debug-logging (command)
  "Run JUnit with COMMAND in Norweb docker container."
  (nl/java-command-in-proj-root
   (nl/java-junit-create-command (format "-Dlogging.level.edu.ualberta.med.biobank=DEBUG -Dtest=\"%s\"" command))))

(defun nl/java-class-name ()
  "Return the name of the class in the buffer where the cursor is in."
  (save-excursion
    (when (re-search-backward java-beginning-of-class-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/java-junit-test-this-file ()
  "For the class the cursor is in, run the Junit test suite."
  (interactive)
  (nl/java-junit-run (nl/java-class-name)))

(defun nl/junit-test-find-method-name ()
  "Return the name of the JUnit test's method. The test method can starts with annotation '@Test'."
  (save-excursion
    (when (re-search-backward junit-test-beginning-regexp)
      (beginning-of-line 2)
      (when (re-search-forward java-function-name-regexp)
        (match-string-no-properties 1))
      )))

(defun nl/java-junit-only-this-method ()
  "Run the JUnit test for the test the cursor is in."
  (interactive)
  (nl/java-junit-run
   (format "%s#%s" (nl/java-class-name) (nl/junit-test-find-method-name))
   ))

(defun nl/java-junit-only-this-method-with-debug-logging ()
  "Run the JUnit test for the test the cursor is in."
  (interactive)
  (nl/java-junit-run-with-debug-logging
   (format "%s#%s" (nl/java-class-name) (nl/junit-test-find-method-name))
   ))

(defun nl/java-test-report-in-chrome ()
  "Open the JUnit report in a Google Chrome tab."
  (interactive)
  (browse-url-chrome (format "file://%starget/site/surefire-report.html" (projectile-project-root))))

(defhydra hydra-nl/java-test (:color blue)
  "Java Test"
  ("f" nl/java-junit-test-this-file "only this file")
  ("d" nl/java-junit-only-this-method-with-debug-logging "only this method")
  ("m" nl/java-junit-only-this-method "only this method")
  ("h" nl/java-junit-html "Generate the JUnit HTML report")
  ("r" nl/java-test-report-in-chrome "Open JUnit report in Chrome"))

(defhydra hydra-nl-java-project (:color red :hint nil)
  "Project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "Java")
  ("t" hydra-nl/java-test/body "test" :color blue :column "Test"))

(key-chord-define java-ts-mode-map "jc" 'hydra-nl-java-project/body)

(define-key java-ts-mode-map (kbd "C-c , d") 'nl/java-junit-only-this-method-with-debug-logging)
(define-key java-ts-mode-map (kbd "C-c , m") 'nl/java-junit-only-this-method)
(define-key java-ts-mode-map (kbd "C-c , f") 'nl/java-junit-test-this-file)
(define-key java-ts-mode-map (kbd "C-c , s") 'nl/java-junit-html)

(provide 'nl-java-project)

;;; nl-java-project.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
