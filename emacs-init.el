;;; package -- Init  -*- lexical-binding: t; -*-
;;; Commentary:

;; Bootstrap config

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))


;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(use-package exec-path-from-shell
  :ensure)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))


(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))


;;; setup-exec-path.el ends here

(setq jit-lock-defer-time 0)


;; TODO: diminish here

(use-package which-key
  :ensure
  :config
  (which-key-mode))




;; Smoother and nicer scrolling
(setq scroll-margin 0
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

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; highlight the current line
(global-hl-line-mode t)

(setq-default cursor-type 'bar)

;; always highlight code
;;(global-font-lock-mode 1)
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

(setq default-directory "~/"
      ;; always follow symlinks when opening files
      vc-follow-symlinks t
      ;; quiet startup
      inhibit-startup-message t
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
      tab-width 4
      )
(setq-default dired-listing-switches "-alh")


;; Remove extra UI clutter by hiding the scrollbar, menubar, and toolbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

(setq ring-bell-function (lambda() ()))

;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
;;(setq split-height-threshold 0)
;;(setq split-width-threshold nil)

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




;;==================
;; Sane defaults
;;==================

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; use human-readable sizes in dired




(global-set-key (kbd "C-x <up>") #'other-window)
(global-set-key (kbd "C-x <down>") #'previous-window-any-frame)

;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
;; (use-package windmove
;;   :ensure
;;   :config
;;   (global-set-key (kbd "<C-s-left>")  'windmove-left)  ;; Ctrl+Cmd+left go to left window
;;   (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window

;;   (global-set-key (kbd "<C-s-right>") 'windmove-right) ;; Ctrl+Cmd+right go to right window
;;   (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window

;;   (global-set-key (kbd "<C-s-up>")    'windmove-up)    ;; Ctrl+Cmd+up go to upper window
;;   (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window

;;   (global-set-key (kbd "<C-s-down>")  'windmove-down)  ;; Ctrl+Cmd+down go to down window
;;   (global-set-key (kbd "s-}")  'windmove-down))        ;; Cmd+Shift+] got to down window


(global-set-key (kbd "M-p") 'ace-window)

(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)

;; both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)


;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)





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

;;(setq max-lisp-eval-depth 10000)

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :ensure
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :ensure
  :init
  (marginalia-mode))

;; Adds intellisense-style code completion at point that works great
;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :ensure
  :init
  (require 'bind-key)
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))




;; Projectile
(use-package projectile
  :ensure
  :init
  (require 'bind-key)
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
   (when (require 'magit nil t)
    (mapc #'projectile-add-known-project
          (mapcar #'file-name-as-directory (magit-list-repos)))
    ;; Optionally write to persistent `projectile-known-projects-file'
    (projectile-save-known-projects))
  )




(use-package projectile-ripgrep
  :ensure
  :after projectile)


(use-package dashboard
  :ensure
  :config
  (dashboard-setup-startup-hook)
  (setq ns-right-alternate-modifier 'none)

  (setq dashboard-items '((recents . 5)
 			  (projects . 20)
			  (bookmarks . 5)
			  ))
  (setq dashboard-navigation-cycle t)
  (setq dashboard-banner-logo-title "Teretulemast!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
  (setq dashboard-projects-backend 'projectile)
  )


;; Show dashboard with emacsclient
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))


(use-package direnv
  :ensure
  :config
  (direnv-mode))

(use-package org
  :ensure)
;; Extended completion utilities
(use-package consult
  :ensure
  :config
  (global-set-key [rebind switch-to-buffer] #'consult-buffer)
  (global-set-key (kbd "C-c j") #'consult-line)
  (global-set-key (kbd "C-c i") #'consult-imenu)
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  )




;; Miscellaneous options
(setq-default major-mode8
              (lambda ()             ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;(setq window-resize-pixelwise t)
;;(setq frame-resize-pixelwise t)



;;===============
;; KeyBindings
;;===============

;; TODO: Map lsp-find-* to something useful

;; Move-text lines around with meta-up/down.
(use-package move-text
  :ensure
  :config
  (move-text-default-bindings))

;;===============
;; UI
;;===============


;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(set-frame-font "Source Code Pro 12")
(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)

(delete-selection-mode t)

(use-package all-the-icons
  :ensure)
(use-package all-the-icons-dired
  :ensure)

(use-package treemacs-all-the-icons
  :ensure)

(use-package doom-themes
  :ensure
  :config
  (load-theme 'doom-solarized-light t)
  (load-theme 'doom-solarized-dark t)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  ;;(enable-theme 'doom-solarized-light)
  (enable-theme 'doom-solarized-dark)
  )

;;(if (display-graphic-p)
    ;;(enable-theme 'doom-solarized-light)
;;    (enable-theme 'doom-solarized-light)
;;  (enable-theme 'doom-solarized-dark))


(use-package solaire-mode
  :ensure
  :config
  (solaire-global-mode +1))







;; Treemacs
(use-package treemacs
  :ensure
  :defer t
  :commands (treemacs)
  :after (lsp-mode)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (define-key treemacs-mode-map [drag-mouse-1] nil)
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                0
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t) ; This causes the focus to jump
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure)

(use-package treemacs-projectile
  :ensure
  :after (treemacs projectile)
  )


;;================
;; Programming
;;================


;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent
  :ensure)

;; ;; Enable autocompletion by default in programming buffers
;;(add-hook 'prog-mode-hook #'corfu-mode)

(use-package jsonrpc
  :ensure)

(use-package copilot
  :ensure
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

;;; Indication of local VCS changes
(use-package diff-hl
  :ensure
  :config
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  )

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

(use-package diff-mode
  :ensure nil
  :mode
  "\\.patch[0-9]*\\'"
  )

()




;; (use-package make-mode
;;   :ensure
;;   :init
;;   (add-to-list 'auto-mode-alist '("Makefile$" . makefile-mode))
;;   (add-to-list 'auto-mode-alist '("makefile$" . makefile-mode))
;;   :mode ("Makefile\\'" "makefile\\'" "Make.obj\\'"))


(use-package cmake-font-lock
  :ensure)

(use-package cmake-mode
  :ensure
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )
(use-package rtags
  :ensure
  )
(require 'subr-x) ; Workaround
(use-package cmake-ide
  :ensure
  :after rtags
  :init
  (require 'rtags)
  (cmake-ide-setup))



(use-package modern-cpp-font-lock
  :ensure
  :hook c++-mode-hook
  )
(use-package google-c-style
  :ensure
  )

(setq-default c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (company-mode)
            (make-local-variable 'standard-indent)
            (setq standard-indent 4)))


;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
  :ensure
  :init
  (require 'bind-key)
  :bind (("C-c g" . magit-status))
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories
        '(;; Directory containing project root directories
          ("~/Developer"      . 2)
          ;; Specific project root directory
          ("~/.emacs.d" . 1)))
  )


(use-package dash
  :ensure)


(use-package markdown-mode
  :ensure
  :after dash)

(use-package markdown-toc
  :ensure
  :after markdown-mode
  )


(use-package yaml-mode
  :ensure)


(use-package highlight-indentation
  :ensure)

;;(use-package sphinx-mode
;;  :ensure
;; )
;;(use-package sphinx-doc
;;  :ensure)

(use-package terraform-mode
  :ensure)



;; lsp-mode
(use-package lsp-mode
  :ensure
  :defines lsp-language-id-configuration
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
	 ;;(terraform-mode . lsp)
	 (go-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (haskell-mode . lsp)
	 (asm-mode . lsp)
	 (shell-script-mode . lsp)
	 (cmake-mode . lsp)
	 (dockerfile-mode . lsp)
	 (html-mode . lsp)
	 (javascript-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (json-mode . lsp)
	 (markdown-mode . lsp)
	 (nix-mode . lsp)

	 )
  :commands lsp
  :custom
  (lsp-use-plists t)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))

  ;; (treemacs-space-between-root-nodes nil)

  (lsp-auto-guess-root t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-before-save-edits nil)
  (lsp-idle-delay 0.3)
  (lsp-completion-provider :capf)
  ;; Prevent constant auto-formatting...)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  ;; be more ide-ish)
  (lsp-headerline-breadcrumb-enable t)
  ;; python-related settings)
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  )
;; Taken from https://tychoish.com/post/emacs-and-lsp-mode/
(use-package lsp-ui
  :ensure
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-alignment 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  ;; :custom-face
  ;; (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :config
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  )

;; Debug
(use-package dap-mode
  :ensure
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :custom
  (dap-auto-configure-mode t)
  (dap-tooltip-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-mode . (lambda () (require 'dap-python)))
         (diff-mode . (lambda () (dap-mode -1)))
	 (go-mode . (lambda () (require 'dap-dlv-go)))
         (powershell-mode . (lambda () (dap-mode -1)))
         (shell-script-mode . (lambda () (dap-mode -1)))
         (cmake-mode . (lambda () (dap-mode -1)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  ;; :bind (:map lsp-mode-map
  ;;        ("M-9" . lsp-treemacs-errors-list))
  )

(use-package diminish

  :ensure)


(use-package company
  :ensure
  :config
  ;; No delay in showing suggestions.
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-tooltip-annotation-padding 1
      company-tooltip-flip-when-above t
      company-text-face-extra-attributes
      '(:weight bold :slant italic)
      )
      (add-hook 'after-init-hook 'global-company-mode)
      )

(diminish 'company-mode)


(use-package flycheck
  :ensure
  :init (global-flycheck-mode))

(diminish 'flycheck-mode)

(use-package dockerfile-mode
  :ensure
  )


(use-package docker-compose-mode
  :ensure
  )


;; EditorConfig
(use-package editorconfig
  :ensure
  :config
  (editorconfig-mode 1)
  )
(use-package editorconfig-generate
  :ensure
  )

(use-package editorconfig-domain-specific
  :ensure
  )

(use-package editorconfig-custom-majormode
  :ensure)


(use-package gitlab-ci-mode
  :ensure
  )

(use-package go-mode
  :ensure
  :after
  (lsp-mode)
  :bind (:map go-mode-map
 	      ("C-c C-f" . 'gofmt))
  ;:hook (before-save . gofmt-before-save)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )


(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(use-package json-snatcher
  :ensure
  )
(use-package json-mode
  :ensure
  :mode "\\.json\\'"
  :after
  (json-snatcher)
  )

;;(load "setup-jupyter")
(use-package nix-mode
  :ensure
  :mode "\\.nix\\'")

;;(load "setup-powershell")


(use-package python
  :ensure
  :after
  (highlight-indentation)
  :init
  (progn
    (add-hook 'python-mode-hook 'highlight-indentation-mode)
    ;; (add-hook 'python-mode-hook 'eldoc-mode)
    ;;    (add-hook 'python-mode-hook 'sphinx-doc-mode))
    )
  :bind (:map python-mode-map
	      (
               ("C-c i" . python-insert-docstring-with-google-style-at-point)
               ))
  :config

  (when (executable-find "ipython")
    (setq
     python-shell-interpreter "ipython"
     python-shell-interpreter-args ""
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
  )

(use-package ruff-format
  :ensure)


(use-package lsp-pyright
  :ensure
  :after lsp-mode


  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  (lsp-pyright-python-executable-cmd "python3")

  )



(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets
  :ensure
  :after (yasnippet))

(use-package tree-sitter
  :ensure
  ;;:ensure-system-package tree-sitter
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  )



;;; init.el ends here
