;;; programming.el --- Programming and development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Programming tools including LSP, tree-sitter, debugging, and language modes.
;; TODO: in HTML mode, Alt+o doesn't work for switching windows
;; TODO: use web-mode
;;; Code:

(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :diminish
  :ensure
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t "Suppress the display of Flymake error counters when there are no errors."))

;; (use-package sideline
;;   :ensure
;;   :hook (prog-mode . sideline-mode))

;; (use-package sideline-flymake
;;  :ensure
;;   :hook (flymake-mode . sideline-flymake-setup))

;; (use-package sideline-eglot
;;   :ensure
;;   :hook (eglot-managed-mode . sideline-eglot-setup))

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(use-package consult-flyspell
  :ensure
  :after (consult flyspell))

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-autoshutdown t)
  :custom
  (eglot-report-progress nil)
  (eglot-extend-to-xref t)
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (flymake-mode 1)))
  (add-hook 'eglot-managed-mode-hook #'eldoc-mode))

(use-package consult-eglot
  :ensure
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :ensure
  :after (consult eglot embark))

(use-package xref
  :init
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

(use-package elisp-lint :ensure)
(use-package package-lint :ensure)

(use-package dtrt-indent
  :diminish
  :ensure
  :hook (prog-mode . dtrt-indent-mode))

(use-package direnv
  :ensure
  :config
  (direnv-mode)
  (add-to-list 'warning-suppress-types '(direnv)))

;; Debugging
(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gdb-debuginfod-enable-setting t))

(use-package dape
  :ensure
  :config
  (dape-breakpoint-global-mode)
  (add-hook 'dape-compile-hook 'kill-buffer)
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)
  (dape-inlay-hints t "Showing inlay hints"))

;; Compilation
(use-package compile-multi
  :ensure
  :config
  (setq compile-multi-config
        '((go-mode . (("go test" . "go test ./...")
                      ("go test current" . "go test .")
                      ("go build" . "go build .")
                      ("just check" . "just check")
                      ("just format" . "just format")))
          (python-mode . (("pytest" . "pytest")
                         ("pytest file" . "pytest %file-name%")
                         ("just check" . "just check")
                         ("just format" . "just format")))
          (haskell-mode . (("stack test" . "stack test")
                          ("cabal test" . "cabal test")
                          ("just check" . "just check")
                          ("just format" . "just format")))
          (nix-ts-mode . (("nix flake check" . "nix flake check")
                         ("nix fmt" . "nix fmt")
                         ("just check" . "just check")
                         ("just format" . "just format")))
          (c++-mode . (("cmake build" . "cmake --build build")
                      ("make" . "make")
                      ("just check" . "just check")
                      ("just format" . "just format")))
          (emacs-lisp-mode . (("elisp-lint" . "just elisp-lint %file-name%")
                             ("just format" . "just format"))))))

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

(use-package wgrep
  :ensure
  :init
  (setq
   wgrep-auto-save-buffer t
   wgrep-change-readonly-file t))

;; Major modes
(use-package gnuplot
  :ensure
  :mode ("\\.plt\\'" . gnuplot-mode))

(use-package markdown-mode
  :after dash
  :ensure
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package conf-mode
  :mode
  (("/.dockerignore\\'" . conf-unix-mode)
   ("/.gitignore\\'" . conf-unix-mode)))

(use-package haskell-mode :ensure)
(use-package diff-mode :ensure :mode "\\.patch[0-9]*\\'")
(use-package terraform-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package docker-compose-mode :ensure)
(use-package gitlab-ci-mode :ensure)
(use-package ansible :ensure)
(use-package ssh-config-mode :ensure)
(use-package adoc-mode :ensure)
(use-package go-mode :ensure)
(use-package nix-ts-mode :ensure :mode "\\.nix\\'")
(use-package cmake-mode :ensure :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package mermaid-mode :ensure)
(use-package yaml-mode :ensure)
(use-package modern-cpp-font-lock :ensure)
(use-package just-mode :ensure)
(use-package sql-indent :ensure)

(use-package xt-mouse
  :custom
  (xterm-mouse-mode 1 "Enable mouse in terminal"))

(use-package vterm
  :ensure
  :custom
  (vterm-always-compile-module t))

(provide 'programming)
;;; programming.el ends here
