;;; Personal configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(package-initialize)


;;(add-to-list 'load-path "~/.emacs.d/lisp")
;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; PERFTUNING


;; Adjust garbage collection threshold for early startup
(setq gc-cons-threshold most-positive-fixnum)

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq jit-lock-defer-time 0)

;; TODO: vterm? https://github.com/sauricat/flakes/blob/154c2261aa9593b10d979924d9e37fb1255ac497/home/emacs/shu/shu-term.el

;; BOOTSTRAP

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
;;(when (file-exists-p custom-file)
;;  (load custom-file))









;;



;; Always load newest byte code
;;(setq load-prefer-newer t)

;; TODO: If not with nix
;;(require 'init-elpa)

;(setq debug-on-error t)
;; Default configs
;; TODO: https://justinbarclay.ca/posts/from-zero-to-ide-with-emacs-and-lsp/

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))






(use-package exec-path-from-shell :ensure)

(use-package emacs

  :init
  (tool-bar-mode -1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))
  (menu-bar-mode -1)
  :custom
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  )

(setq
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t)

(setq-default c-basic-offset 4)


(use-package powershell
  :ensure)


(use-package centered-cursor-mode
  :ensure
  :config
  ;; Optional, enables centered-cursor-mode in all buffers.
  (global-centered-cursor-mode))



;; Smoother and nicer scrolling
(setq
 scroll-margin 0
 scroll-step 1
 next-line-add-newlines nil
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq column-number-mode t)

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


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
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 ;; when quiting emacs, just kill processes
 confirm-kill-processes nil
 ;; ask if local variables are safe once.
 enable-local-variables t
 ;; life is too short to type yes or no
 use-short-answers t
 tab-width 4)

(setq backup-directory-alist
           `(("." . "~/.emacs-saves")))
;;(setq auto-save-file-name-transforms   `((".*",(concat user-emacs-directory "auto-saves") t)))

(defun my/diff-auto-save-file ()
      "Get auto-save #file# difference with current buffer."
      (interactive)
      (diff (make-auto-save-file-name) (current-buffer) nil 'noasync))


(setq-default dired-listing-switches "-alh")

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

(setq ring-bell-function (lambda () ()))

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode)

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



;;(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)

(delete-selection-mode t)

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

;; Define super modifier key. This is sometimes neededn but it's not defined
(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)

(when (eq system-type 'darwin)
  (require 'init-macos))


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist '((c-mode    . c-ts-mode)
                            (c++-mode  . c++-ts-mode)
                            (go-mode   . go-ts-mode)
                            (html-mode . html-ts-mde)
                            (bash-mode . bash-ts-mode)
                            (python-mode . python-ts-mode)
			    (typescript-mode . typescript-ts-mode)
                            ))
  )


;; Move current line or region by olding Meta-Up and Meta-Down
(use-package move-text
  :ensure
  :init
  (move-text-default-bindings))

;; Highlight comments
   (use-package hl-todo
     :hook
     (prog-mode . hl-todo-mode)
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

(use-package terraform-mode
  :ensure)

(require 'init-minibuffer)

(use-package
 orderless
 :ensure
 :config
 (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if
	(and (boundp 'consult--tofu-char)
	     (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp
	. ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp
	. ,(concat "\\." (substring word 1)
		   (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles
     '(orderless-initialism orderless-literal orderless-regexp)))



  ;; Certain dynamic completion tables (completion-table-dynamic) do not work
  ;; properly with orderless. One can add basic as a fallback.  Basic will only
  ;; be used when orderless fails, which happens only for these special
  ;; tables. Also note that you may want to configure special styles for special
  ;; completion categories, e.g., partial-completion for files.
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides
	'((file (styles partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command
					 (styles
					  +orderless-with-initialism))
                                        (variable
					 (styles
					  +orderless-with-initialism))
                                        (symbol
					 (styles
					  +orderless-with-initialism))
					(eglot (styles orderless))
                                      (eglot-capf (styles orderless)))
        orderless-component-separator
	#'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers (list
				     #'+orderless-consult-dispatch
                                          #'orderless-affix-dispatch))
 ;;:custom
 ;; Configure a custom style dispatcher (see the Consult wiki)
 ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
 ;; (orderless-component-separator #'orderless-escapable-split-on-space)
 ;(completion-styles '(substring orderless basic))
 ;(completion-category-defaults nil)
; (completion-category-overrides
 ; '((file (styles partial-completion))))
)




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


(use-package expand-region
  :ensure
   :bind ("C-=" . er/expand-region))

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

(use-package web-mode
  :ensure)

(use-package vala-mode
  :ensure)



(require 'init-eglot)

(use-package consult-yasnippet :ensure)

(use-package typescript-mode
  :ensure)


;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(use-package all-the-icons :ensure)
(use-package all-the-icons-dired :ensure)
;;(use-package treemacs-all-the-icons :ensure)

(use-package
 leuven-theme
 :ensure
 :config
 ;;(load-theme 'leuven t)
 (load-theme 'leuven-dark t)
 )

(use-package ace-link :ensure :config (ace-link-setup-default))

(use-package solaire-mode :ensure :config (solaire-global-mode +1))

(setq window-resize-pixelwise t)

;; Treemacs
;; (use-package
;;  treemacs
;;  :ensure
;;  ;;:defer t
;;  :commands (treemacs)
;;  :init
;;  (with-eval-after-load 'winum
;;    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;  :config
;;  ;;(define-key treemacs-mode-map [drag-mouse-1] nil)
;;  (progn
;;    (setq
;;     treemacs-collapse-dirs (if treemacs-python-executable 3 0)
;;     treemacs-deferred-git-apply-delay 0.5
;;     treemacs-directory-name-transformer #'identity
;;     treemacs-display-in-side-window t
;;     treemacs-eldoc-display 'simple
;;     treemacs-file-event-delay 2000
;;     treemacs-file-extension-regex treemacs-last-period-regex-value
;;     treemacs-file-follow-delay 0.2
;;     treemacs-file-name-transformer #'identity
;;     treemacs-follow-after-init t
;;     treemacs-expand-after-init t
;;     treemacs-find-workspace-method 'find-for-file-or-pick-first
;;     treemacs-git-command-pipe ""
;;     treemacs-goto-tag-strategy 'refetch-index
;;     treemacs-header-scroll-indicators '(nil . "^^^^^^")
;;     treemacs-hide-dot-git-directory t
;;     treemacs-indentation 2
;;     treemacs-indentation-string " "
;;     treemacs-is-never-other-window nil
;;     treemacs-max-git-entries 5000
;;     treemacs-missing-project-action 'ask
;;     treemacs-move-forward-on-expand nil
;;     treemacs-no-png-images nil
;;     treemacs-no-delete-other-windows t
;;     treemacs-project-follow-cleanup nil
;;     treemacs-persist-file
;;     (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;     treemacs-position 'left
;;     treemacs-read-string-input 'from-child-frame
;;     treemacs-recenter-distance 0.1
;;     treemacs-recenter-after-file-follow nil
;;     treemacs-recenter-after-tag-follow nil
;;     treemacs-recenter-after-project-jump 'always
;;     treemacs-recenter-after-project-expand 'on-distance
;;     treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
;;     treemacs-project-follow-into-home nil
;;     treemacs-show-cursor nil
;;     treemacs-show-hidden-files t
;;     treemacs-silent-filewatch nil
;;     treemacs-silent-refresh nil
;;     treemacs-sorting 'alphabetic-asc
;;     treemacs-select-when-already-in-treemacs 'move-back
;;     treemacs-space-between-root-nodes t
;;     treemacs-tag-follow-cleanup t
;;     treemacs-tag-follow-delay 0
;;     treemacs-text-scale nil
;;     treemacs-user-mode-line-format nil
;;     treemacs-user-header-line-format nil
;;     treemacs-wide-toggle-width 70
;;     treemacs-width 35
;;     treemacs-width-increment 1
;;     treemacs-width-is-initially-locked t
;;     treemacs-workspace-switch-cleanup nil)
;;    ;; The default width and height of the icons is 22 pixels. If you are
;;    ;; using a Hi-DPI display, uncomment this to double the icon size.
;;    ;;(treemacs-resize-icons 44)
;;    (treemacs-follow-mode t)
;;    ; This causes the focus to jump
;;    (treemacs-filewatch-mode t)
;;    (treemacs-fringe-indicator-mode 'always)
;;    (when treemacs-python-executable
;;      (treemacs-git-commit-diff-mode t))
;;    (pcase (cons
;;            (not (null (executable-find "git")))
;;            (not (null treemacs-python-executable)))
;;      (`(t . t) (treemacs-git-mode 'deferred))
;;      (`(t . _) (treemacs-git-mode 'simple)))
;;    (treemacs-hide-gitignored-files-mode nil))
;;  :bind
;;  (:map
;;   global-map
;;   ("M-0" . treemacs-select-window)
;;   ("C-x t 1" . treemacs-delete-other-windows)
;;   ("C-x t t" . treemacs)
;;   ("C-x t d" . treemacs-select-directory)
;;   ("C-x t B" . treemacs-bookmark)
;;   ("C-x t C-t" . treemacs-find-file)
;;   ("C-x t M-t" . treemacs-find-tag)))



;; Make script file executable by default
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; (use-package
;;  treemacs-icons-dired
;;  :hook
;;  (dired-mode . treemacs-icons-dired-enable-once)
;;  :ensure)


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

;; (use-package
;;  cmake-ide
;;  :ensure
;;  :after rtags
;;  :init
;;  (require 'rtags)
;;  (cmake-ide-setup))
;; TODO: https://github.com/redguardtoo/cpputils-cmake
(use-package modern-cpp-font-lock :ensure :hook c++-mode-hook)

(use-package google-c-style :ensure)

(require 'init-git)
(use-package deadgrep :ensure)

(use-package dash :ensure)

(use-package docker :ensure :bind ("C-c d" . docker))

(use-package markdown-mode :ensure :after dash)

(use-package markdown-toc :ensure :after markdown-mode)

(use-package yaml-mode :ensure)

(use-package highlight-indentation :ensure)

(require 'init-corfu)

;; Add extensions
(use-package cape
  :ensure
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )



(use-package eldoc
  :init
  (global-eldoc-mode))

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

(require 'init-flymake)



(use-package dockerfile-mode :ensure)

(use-package docker-compose-mode :ensure)
;; EditorConfig
(use-package editorconfig :ensure :config (editorconfig-mode 1))

(use-package editorconfig-generate :ensure)

(use-package editorconfig-domain-specific :ensure)

(use-package editorconfig-custom-majormode :ensure)

(use-package gitlab-ci-mode :ensure)

(use-package avy
:ensure
:config
(global-set-key (kbd "C-c z") #'avy-goto-word-1)
(setq avy-all-windows 'all-frames))

(use-package
 go-mode
 :ensure
 :bind (:map go-mode-map ("C-c C-f" . 'gofmt))
 ;:hook (before-save . gofmt-before-save)
 :config (add-hook 'go-mode-hook #'yas-minor-mode))

(use-package json-snatcher :ensure)

(use-package terraform-mode
  :ensure)

(use-package
 json-mode
 :ensure
 :mode "\\.json\\'"
 :after (json-snatcher))
(require 'init-nix)

(require 'init-python)


(use-package
 yasnippet
 :ensure
 :config (yas-reload-all) (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets :ensure :after (yasnippet))


;; Schema

(use-package paredit
  :ensure)

(use-package racket-mode :ensure)
(use-package geiser :ensure)
(use-package geiser-racket :ensure)
(use-package geiser-guile :ensure)


;; Local Variables:
;; bute-compile-warnings: (not free-vars)
;; End:
