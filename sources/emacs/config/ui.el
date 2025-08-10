;;; ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; UI configuration including themes, fonts, and visual enhancements.

;;; Code:

(use-package emacs
  :init
  (load-theme 'leuven t)
  (load-theme 'leuven-dark t t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package diminish :ensure)

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

(use-package hl-line
  :custom
  (global-hl-line-sticky-flag t)
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode org-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package rainbow-delimiters
  :ensure
  :hook((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package auto-dark
  :ensure
  :diminish
  :after leuven-theme
  :custom
  (auto-dark-themes '((leuven-dark) (leuven)))
  :config
  (auto-dark-mode 1)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame (auto-dark-mode 1))))))

(use-package calendar
  :config
  (copy-face 'font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setq calendar-week-start-day 1)
  (setq calendar-intermonth-text
        '(propertize (format "%2d" (car (calendar-iso-from-absolute
                                         (calendar-absolute-from-gregorian (list month day year)))))
                     'font-lock-face 'calendar-iso-week-face)))

(use-package breadcrumb
  :ensure
  :init
  (breadcrumb-mode))

(use-package nerd-icons :ensure)

(use-package nerd-icons-corfu
  :ensure
  :after nerd-icons corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-ibuffer
  :ensure
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package hl-todo
  :ensure
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold))))

(use-package sr-speedbar :ensure)

(provide 'ui)
;;; ui.el ends here