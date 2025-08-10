;;; help.el --- Help and documentation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced help and documentation tools.

;;; Code:

(use-package help-at-pt
  :custom
  (help-at-pt-display-when-idle t)) ; Display messages when idle, without prompting

(use-package helpful
  :ensure
  :diminish
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)
   ([remap describe-symbol]   . helpful-symbol)
   ("C-h C" . #'helpful-command)))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package dash-docs
  :ensure
  :defines (dash-docs-docsets dash-docs-docsets-path)
  :custom
  (dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/")
  :config
  (dolist (spec '((emacs-lisp-mode . ("Emacs Lisp"))
                  (python-mode . ("Python 3"))
                  (js-mode . ("JavaScript" "NodeJS"))
                  (go-mode . ("Go"))
                  (c++-mode . ("C++"))
                  (java-mode . ("Java SE"))
                  (ruby-mode . ("Ruby"))
                  (haskell-mode . ("Haskell"))))
    (add-hook (intern (concat (symbol-name (car spec)) "-hook"))
              (let ((docsets (cdr spec)))
                (lambda () (setq-local dash-docs-docsets docsets))))))

(use-package consult-dash
  :after dash-docs
  :config
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))

(provide 'help)
;;; help.el ends here