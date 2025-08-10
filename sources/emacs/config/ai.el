;;; ai.el --- AI and LLM integrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered tools and language model integrations.

;;; Code:

(use-package aidermacs :ensure)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :defer t
  :custom
  ;; Suppress warnings and improve performance
  (claude-code-ide-terminal-initialization-delay 0.5)
  :init
  ;; Lazy load on first use
  (autoload 'claude-code-ide-menu "claude-code-ide" "Open Claude Code IDE menu" t)
  :config
  ;; Suppress byte-compilation warnings
  (with-no-warnings
    (claude-code-ide-emacs-tools-setup)))

(provide 'ai)
;;; ai.el ends here