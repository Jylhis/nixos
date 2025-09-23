;;; ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; UI configuration including themes, fonts, and visual enhancements.

;;; Code:

(use-package emacs
  :init
  ;; Trust all themes by default without prompting
  (setq custom-safe-themes t)
  ;; (load-theme 'leuven t)
  ;; (load-theme 'leuven-dark t t)
  (load-theme 'modus-vivendi-tinted t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package modus-themes
  :ensure
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui nil)
  :bind ("C-c t" . modus-themes-toggle)
  :config
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  )

(use-package auto-dark
  :ensure
  :diminish
  :after modus-themes
  :custom
  (auto-dark-themes '((modus-operandi-tinted) (modus-vivendi-tinted)))
  :config
  (auto-dark-mode 1)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame (auto-dark-mode 1))))))

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
  :hook (prog-mode . breadcrumb-mode))

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

;; Essential built-in enhancements
(use-package winner
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :config
  (winner-mode 1))

 (use-package pixel-scroll
   :when (display-graphic-p)
   :config
   (pixel-scroll-precision-mode 1)) ;; TODO: Does this work with server-client. Check auto-dark config

(use-package emojify
  :ensure
  :custom
   (emojify-inhibit-major-modes '(dired-mode
                                 doc-view-mode
                                 debugger-mode
                                 pdf-view-mode
                                 image-mode
                                 help-mode
                                 ibuffer-mode
                                 magit-popup-mode
                                 magit-diff-mode
                                 nix-mode
                                 ert-results-mode
                                 compilation-mode
                                 proced-mode
                                 mu4e-headers-mode
                                 deft-mode
                                 yaml-mode
                                 prog-mode))

  ;; :hook (after-init . global-emojify-mode)
   )

(provide 'ui)
;;; ui.el ends here
