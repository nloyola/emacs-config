;;; nl-react --- package to help with development in React projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-when-compile
  (require 'yasnippet))

(defun nl/yas-reload-all ()
  "Reload all snippets, including custom ones."
  (interactive)
  (yas-reload-all)
  (yas--load-directory-1 "~/.emacs.d/react/snippets" 'tsx-ts-mode))


(provide 'nl-react)
;;; nl-react.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
