;;; per-project.el --- Enhance projects -*- lexical-binding: t; -*-

;;; Commentary:
;; Tools and configs to work with projects
;;; Code:

(use-package project
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  ;; (project-vc-extra-root-markers '(
  ;; 				   "go.mod"
  ;; 				   "package.json"
  ;; 				   "justfile"
  ;; 				   "package.xml"
  ;; 				   ))
  )

;; TODO: Add more types: https://github.com/mohkale/projection/tree/master?tab=readme-ov-file#projection-commands
(use-package projection
  :ensure
  ;; Enable the `projection-hook' feature.
  :hook (after-init . global-projection-hook-mode)

  ;; Require projections immediately after project.el.
  :config
  (with-eval-after-load 'project
    (require 'projection))

  :config
  ;; Uncomment if you want to disable prompts for compile commands customized in .dir-locals.el
  (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  (put 'projection-commands-install-project 'safe-local-variable #'stringp)

  ;; Access pre-configured projection commands from a keybinding of your choice.
  ;; Run `M-x describe-keymap projection-map` for a list of available commands.
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :ensure t
  ;; Allow interactively selecting available compilation targets from the current
  ;; project type.
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :ensure t
  :after embark
  :after projection-multi
  :demand t
  ;; Add the projection set-command bindings to `compile-multi-embark-command-map'.
  :config (projection-multi-embark-setup-command-map))

;; Compilation
(use-package compile-multi
  :ensure
  :config
  (setq compile-multi-config
        '((go-mode . (("go test" . "go test ./...")
                      ("go test current" . "go test .")
                      ("go build" . "go build .")))
          (python-mode . (("pytest" . "pytest")
                         ("pytest file" . "pytest %file-name%")))
          (haskell-mode . (("stack test" . "stack test")
                          ("cabal test" . "cabal test")))
          (nix-ts-mode . (("nix flake check" . "nix flake check")
                         ("nix fmt" . "nix fmt")))
          )))

(use-package consult-compile-multi
  :ensure
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :ensure
  :after nerd-icons-completion
  :after compile-multi
  :demand t)

(use-package compile-multi-embark
  :ensure
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

(provide 'per-project)
;;; per-project.el ends here
