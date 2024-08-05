;;; package -- Init  -*- lexical-binding: t; -*-
;;; Commentary:

;; Default configs

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; Custom file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-readable-p custom-file)
  (load custom-file))

;; Adjust garbage collection threshold for early startup
(setq gc-cons-threshold (* 128 1024 1024))
;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
;;(setq process-adaptive-read-buffering nil)
(use-package exec-path-from-shell :ensure)
;; (with-eval-after-load 'exec-path-from-shell
;;   (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;     (add-to-list 'exec-path-from-shell-variables var)))

(setq
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t)

(setq-default c-basic-offset 4)

;; Smoother and nicer scrolling
(setq
 scroll-margin 0
 scroll-step 1
 next-line-add-newlines nil
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Don't bother with auto save and backups.
(setq auto-save-default nil)

(setq make-backup-files nil)

;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; highlight the current line
(global-hl-line-mode t)

(setq-default cursor-type 'bar)

;; always highlight code
(global-font-lock-mode 1)

;; refresh a buffer if changed on disk
(global-auto-revert-mode t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(setq require-final-newline t)

(setq
 default-directory "~/"
 ;; always follow symlinks when opening files
 vc-follow-symlinks t
 ;; quiet startup
 inhibit-startup-message t
 inhibit-splash-screen t
 initial-scratch-message nil
 inhibit-startup-screen t
 ;; hopefully all themes we install are safe
 custom-safe-themes t
 ;; simple lock/backup file management
 create-lockfiles nil
 backup-by-copying t
 delete-old-versions t
 ;; when quiting emacs, just kill processes
 confirm-kill-processes nil
 ;; ask if local variables are safe once.
 enable-local-variables t
 ;; life is too short to type yes or no
 use-short-answers t
 backup-directory-alist '(("." . "~/MyEmacsBackups"))
 tab-width 4)

(setq-default dired-listing-switches "-alh")
;; Remove extra UI clutter by hiding the scrollbar, menubar, and toolbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

(setq ring-bell-function (lambda () ()))

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)

;; Keep track of open files
(recentf-mode t)

;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

;; Display line numbers only when in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Smarter move beginning of the line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; Set font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)

(delete-selection-mode t)

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

;; Define super modifier key. This is sometimes neededn but it's not defined
(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac specific stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'darwin)
  ;; both command keys are 'Super'
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'super)

  ;; Option or Alt is naturally 'Meta'
  (setq mac-option-modifier 'meta)


  ;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
  (setq mac-right-option-modifier 'nil)
  ;; Enable transparent title bar on macOS
  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  )


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;
(require 'treesit)

;; Move current line or region by olding Meta-Up and Meta-Down
(use-package move-text :ensure :config (move-text-default-bindings))

;; Highlight comments
  (use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
    :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       warning bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold))))

;; TODO: This has been integrated with emacs, possibly released with emacs 30
(use-package which-key :ensure :config (which-key-mode))


;; Move between windows with numbers C-x w <num>
(use-package winum :ensure :config (winum-mode))


;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package
 vertico
 :ensure
 :custom
 (vertico-cycle t)
 (read-buffer-completion-ignore-case t)
 (read-file-name-completion-ignore-case t)
 ;;(completion-styles '(basic substring partial-completion flex))
 :init (vertico-mode))

(use-package
 orderless
 :ensure
 :custom
 ;; Configure a custom style dispatcher (see the Consult wiki)
 ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
 ;; (orderless-component-separator #'orderless-escapable-split-on-space)
 (completion-styles '(substring orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides
  '((file (styles partial-completion)))))


;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package
 marginalia
 :after vertico
 :ensure
 :init (marginalia-mode))


;; Adds intellisense-style code completion at point that works great
;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package
 helpful
 :ensure
 :init
 ;(require 'helpful)
 (require 'bind-key)
 :bind
 (("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-c C-d" . #'helpful-at-point)
  ("C-h F" . #'helpful-function)
  ("C-h C" . #'helpful-command)))


(use-package direnv :ensure :config (direnv-mode))

(use-package org :ensure
  :bind
  (("C-c l" . #'org-store-link)
   ("C-c a" . #'org-agenda)
   ("C-c c" . #'org-capture))
  :config
  (setq org-tag-alist '(
			("@work" . ?w)
			("@home" . ?h)
			)))

	(setq org-directory "~/Org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
      (setq org-link-descriptive t)
  

;; Extended completion utilities
(use-package
 consult
 :ensure
 :config
 (global-set-key [rebind switch-to-buffer] #'consult-buffer)
 (global-set-key (kbd "C-c j") #'consult-line)
 (global-set-key (kbd "C-c i") #'consult-imenu)
 (setq
  read-buffer-completion-ignore-case t
  read-file-name-completion-ignore-case t
  completion-ignore-case t))
;; Miscellaneous options
(setq-default major-mode8
              (lambda ()
                ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(use-package all-the-icons :ensure)
(use-package all-the-icons-dired :ensure)
(use-package treemacs-all-the-icons :ensure)

(use-package
 leuven-theme
 :ensure
 :config
 ;;(load-theme 'leuven-theme t)
 (load-theme 'leuven-dark t)
 ;;(enable-theme 'leuven-theme)
 )

(use-package ace-link :ensure :config (ace-link-setup-default))

(use-package solaire-mode :ensure :config (solaire-global-mode +1))

;; Treemacs
(use-package
 treemacs
 :ensure
 ;;:defer t
 :commands (treemacs)
 :init
 (with-eval-after-load 'winum
   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
 :config
 ;;(define-key treemacs-mode-map [drag-mouse-1] nil)
 (progn
   (setq
    treemacs-collapse-dirs (if treemacs-python-executable 3 0)
    treemacs-deferred-git-apply-delay 0.5
    treemacs-directory-name-transformer #'identity
    treemacs-display-in-side-window t
    treemacs-eldoc-display 'simple
    treemacs-file-event-delay 2000
    treemacs-file-extension-regex treemacs-last-period-regex-value
    treemacs-file-follow-delay 0.2
    treemacs-file-name-transformer #'identity
    treemacs-follow-after-init t
    treemacs-expand-after-init t
    treemacs-find-workspace-method 'find-for-file-or-pick-first
    treemacs-git-command-pipe ""
    treemacs-goto-tag-strategy 'refetch-index
    treemacs-header-scroll-indicators '(nil . "^^^^^^")
    treemacs-hide-dot-git-directory t
    treemacs-indentation 2
    treemacs-indentation-string " "
    treemacs-is-never-other-window nil
    treemacs-max-git-entries 5000
    treemacs-missing-project-action 'ask
    treemacs-move-forward-on-expand nil
    treemacs-no-png-images nil
    treemacs-no-delete-other-windows t
    treemacs-project-follow-cleanup nil
    treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
    treemacs-position 'left
    treemacs-read-string-input 'from-child-frame
    treemacs-recenter-distance 0.1
    treemacs-recenter-after-file-follow nil
    treemacs-recenter-after-tag-follow nil
    treemacs-recenter-after-project-jump 'always
    treemacs-recenter-after-project-expand 'on-distance
    treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
    treemacs-project-follow-into-home nil
    treemacs-show-cursor nil
    treemacs-show-hidden-files t
    treemacs-silent-filewatch nil
    treemacs-silent-refresh nil
    treemacs-sorting 'alphabetic-asc
    treemacs-select-when-already-in-treemacs 'move-back
    treemacs-space-between-root-nodes t
    treemacs-tag-follow-cleanup t
    treemacs-tag-follow-delay 0
    treemacs-text-scale nil
    treemacs-user-mode-line-format nil
    treemacs-user-header-line-format nil
    treemacs-wide-toggle-width 70
    treemacs-width 35
    treemacs-width-increment 1
    treemacs-width-is-initially-locked t
    treemacs-workspace-switch-cleanup nil)
   ;; The default width and height of the icons is 22 pixels. If you are
   ;; using a Hi-DPI display, uncomment this to double the icon size.
   ;;(treemacs-resize-icons 44)
   (treemacs-follow-mode t)
   ; This causes the focus to jump
   (treemacs-filewatch-mode t)
   (treemacs-fringe-indicator-mode 'always)
   (when treemacs-python-executable
     (treemacs-git-commit-diff-mode t))
   (pcase (cons
           (not (null (executable-find "git")))
           (not (null treemacs-python-executable)))
     (`(t . t) (treemacs-git-mode 'deferred))
     (`(t . _) (treemacs-git-mode 'simple)))
   (treemacs-hide-gitignored-files-mode nil))
 :bind
 (:map
  global-map
  ("M-0" . treemacs-select-window)
  ("C-x t 1" . treemacs-delete-other-windows)
  ("C-x t t" . treemacs)
  ("C-x t d" . treemacs-select-directory)
  ("C-x t B" . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)))

(use-package
 treemacs-icons-dired
 :hook (dired-mode . treemacs-icons-dired-enable-once)
 :ensure)

(use-package treemacs-magit :after (treemacs magit) :ensure)

;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent :ensure)

;;; Indication of local VCS changes
(use-package
 diff-hl
 :ensure
 :config (add-hook 'prog-mode-hook #'diff-hl-mode))

(use-package diff-mode :ensure nil :mode "\\.patch[0-9]*\\'")

(use-package cmake-font-lock :ensure)

(use-package
 cmake-mode
 :ensure
 :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package rtags :ensure)

(use-package
 cmake-ide
 :ensure
 :after rtags
 :init
 (require 'rtags)
 (cmake-ide-setup))

(use-package modern-cpp-font-lock :ensure :hook c++-mode-hook)

(use-package google-c-style :ensure)

;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package
 magit
 :ensure
 :init (require 'bind-key)
 :bind (("C-c g" . magit-status))
 :config
 ;; Show word-granularity differences within diff hunks
 (setq magit-diff-refine-hunk t)
 (setq magit-repository-directories
       '( ;; Directory containing project root directories
         ("~/Developer" . 2)
         ;; Specific project root directory
         ("~/nixos-config" . 1))))

(use-package magit-lfs :ensure :after magit)

(use-package
 magit-todos
 :ensure
 :after magit
 :config (magit-todos-mode 1))

(use-package deadgrep :ensure)

(use-package dash :ensure)

(use-package docker :ensure :bind ("C-c d" . docker))

(use-package markdown-mode :ensure :after dash)

(use-package markdown-toc :ensure :after markdown-mode)

(use-package yaml-mode :ensure)

(use-package highlight-indentation :ensure)

;;(use-package terraform-mode :ensure)

(use-package
 company
 :ensure
 :bind ("M-m" . company-complete)
 :init (add-hook 'after-init-hook 'global-company-mode)
 ;;:hook (prog-mode . company-mode)
 :config
 (setq
  company-idle-delay 0.2
  company-minimum-prefix-length 3
  company-selection-wrap-around t
  company-tooltip-align-annotations t
  company-tooltip-annotation-padding 1
  company-tooltip-flip-when-above t
  company-dabbrev-code-other-buffers t
  company-dabbrev-other-buffers t
  company-dabbrev-ignore-case t
  company-text-face-extra-attributes '(:weight bold :slant italic))
 )

(use-package
 eglot
 :ensure
 :hook
 (python-mode . eglot-ensure)
 (c-mode . eglot-ensure)
 (c++-mode . eglot-ensure)
 (nix-mode . eglot-ensure))

(use-package flymake :ensure)

(use-package dockerfile-mode :ensure)

(use-package docker-compose-mode :ensure)
;; EditorConfig
(use-package editorconfig :ensure :config (editorconfig-mode 1))

(use-package editorconfig-generate :ensure)

(use-package editorconfig-domain-specific :ensure)

(use-package editorconfig-custom-majormode :ensure)

(use-package gitlab-ci-mode :ensure)

(use-package
 go-mode
 :ensure
 :bind (:map go-mode-map ("C-c C-f" . 'gofmt))
 ;:hook (before-save . gofmt-before-save)
 :config (add-hook 'go-mode-hook #'yas-minor-mode))

(use-package json-snatcher :ensure)

(use-package
 json-mode
 :ensure
 :mode "\\.json\\'"
 :after (json-snatcher))

(use-package nix-mode :ensure :mode "\\.nix\\'")

(use-package ruff-format :ensure)

(use-package
 yasnippet
 :ensure
 :config (yas-reload-all) (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets :ensure :after (yasnippet))
