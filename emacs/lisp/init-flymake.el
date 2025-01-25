;; Enabled inline static analysis
(use-package flymake
  :ensure
  :init
  :hook (prog-mode . flymake-mode)
  )

;; Enable the use of flycheck checks with flymake
;; (use-package flymake-flycheck :ensure
;;  :hook
;;  (flymake-mode-hook . flymake-flycheck-auto))

(use-package flymake-vala
  :ensure)
(use-package flymake-shellcheck
  :ensure
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))
(use-package flymake-racket :ensure)
(use-package flymake-guile :ensure)
;;(use-package flymake-go :ensure)

;; https://github.com/ROCKTAKEY/flymake-elisp-config
(use-package flymake-elisp-config
  :ensure)

(use-package flymake-ansible-lint :ensure)


(provide 'init-flymake)
