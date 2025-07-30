;; -*- lexical-binding: t; -*-
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; https://github.com/svaante/dape?tab=readme-ov-file#performance
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Ideas:
;; https://github.com/mohkale/compile-multi
;; https://github.com/dash-docs-el/dash-docs OR https://github.com/astoff/devdocs.el
;; https://github.com/mclear-tools/consult-notes
;; https://github.com/org-roam/org-roam
;; https://github.com/Qkessler/consult-project-extra/
;; https://github.com/emacs-citar/citar

;; DEBUGGING:
;; Start debugger on specific message
;; (setq debug-on-message "standard-indent adjusted")

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

(setq load-prefer-newer t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Hide minor modes from modeline
(use-package diminish :ensure)

;; Builtin stuff
(use-package ffap
  :custom
  (ffap-machine-p-known 'reject)) ; Don’t attempt to ping unknown hostnames


(use-package emacs
  :custom
  ;;   (scroll-margin 15 "Keep 15 line margin from top and bottom")
  ;; (scroll-conservatively 10000)
  ;; (scroll-preserve-screen-position 1 "keep the cursor in the same position while scrolling")
  ;; (blink-matching-paren-highlight-offscreen t) ;; ORGANIZE
  (text-mode-ispell-word-completion nil "Emacs 30 and newer: Disable Ispell completion function.")
  (context-menu-mode t "Enable context menu for vertico")
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (use-short-answers t "life is too short to type yes or no")
  (create-lockfiles nil)
  (delete-by-moving-to-trash t "Delete by moving to trash in interactive mode")
  (sentence-end-double-space nil "Disable the obsolete practice of end-of-line spacing from the typewriter era.")
  (save-place-mode t "Automatically save your place in files")
  (word-wrap t "Continue wrapped lines at whitespace rather than breaking in the middle of a word.")
  (visible-bell nil "No blinking")
  (ring-bell-function #'ignore "No beeping")
  (scroll-preserve-screen-position 1 "keep the cursor in the same position while scrolling")
  (enable-recursive-minibuffers t "Support opening new minibuffers from inside existing minibuffers")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt) "Do not allow the cursor in the minibuffer prompt")
  ;; Ignore case
  (completion-ignore-case t "Don't consider case significant in completion")
  (read-buffer-completion-ignore-case t "Ignore case when reading buffer name")
  (read-file-name-completion-ignore-case t "Ignore case for file completion")
  :bind
  (
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window)
   ("M-p" . scroll-down-line)
   ("M-n" . scroll-up-line)
   )
  :init
  (load-theme 'leuven t)
  (load-theme 'leuven-dark t t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)  
  )

(use-package window
  :custom
  ;; Prefer side by side splitting
  (split-width-threshold 170)
  (split-height-threshold nil))

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))

(use-package apropos
  :custom
  (apropos-do-all t))

(use-package hl-line
  :custom
  (global-hl-line-sticky-flag t)
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode org-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package xt-mouse
  :custom
  (xterm-mouse-mode 1 "Enable mouse in terminal"))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package ibuffer
  :bind
  (([remap list-buffers] . ibuffer)))

(use-package repeat
  :config
  (repeat-mode))

(use-package calendar
  :config
  (copy-face 'font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setq calendar-week-start-day 1)
  (setq calendar-intermonth-text
        '(propertize (format "%2d" (car (calendar-iso-from-absolute
                                         (calendar-absolute-from-gregorian (list month day year)))))
                     'font-lock-face 'calendar-iso-week-face)))


(use-package files
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p) ; Make script file executable by default
  :custom
  ;; Disable autosave and backups
  (auto-save-default nil "Disable separate autosave files")
  (make-backup-files nil "Disable auto backup files")
  (find-file-visit-truename t "Resolve symlinks")
  (confirm-kill-processes nil "when quitting emacs, just kill processes")
  (enable-local-variables t "ask if local variables are safe once")
  )

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Automatically refrech buffer if changed on disk")
  (global-auto-revert-non-file-buffers t "Revert also non-file buffers"))


(use-package recentf
  :custom
  (recentf-max-saved-items 50 "Increase the default a bit")
  (recentf-mode t "Keep track of open files"))


(use-package delsel
  :hook (after-init . delete-selection-mode))


(use-package xref
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  )

(use-package simple
  :custom
  (read-extended-command-predicate
   #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode")
  (kill-do-not-save-duplicates t "Remove duplicates from the kill ring to reduce clutter")
  (line-number-mode t "Show line number in modeline")
  (column-number-mode t "Show column number")

  )


;; (use-package elec-pair
;;   ;; REVIEW(alternative): paren
;;   :diminish
;;   :hook (after-init . electric-pair-mode))


(use-package org
  :custom
  (org-directory "~/Documents")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-startup-with-inline-images t)
  (org-html-html5-fancy t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config
  (setq org-agenda-files (mapcar 'file-truename (file-expand-wildcards (concat org-directory "/**/*.org"))))
  ;; REVIEW: (setq org-agenda-files (directory-files-recursively "~/Documents/org" "\\.org$"))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  :hook (org-mode . visual-line-mode)
  :bind (
    	 ("C-c a" . org-agenda)
    	 ("C-c c" . org-capture))
  )

(use-package org-appear
  :ensure
  :hook
  (org-mode . org-appear-mode)
  :after org)


(use-package org-modern
  :ensure
  :after org
  :hook
  (org-mode . global-org-modern-mode)
  )


(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  )

(use-package elisp-mode
  :dash "Emacs_Lisp"
  )


;; QOL stuff

(use-package dash-docs
  :ensure
  :custom
  (dash-docs-docsets-path "~/.local/share/Zeal/Zeal/docsets/")
  )

(use-package consult-dash
  :bind (("M-s d" . consult-dash))
  :after dash-docs
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))

(use-package avy 
  :ensure
  :bind (
   	 ("M-g c"   . avy-goto-char) ;; Orig: goto-char; other options -2 and -timer
   	 ("M-g l" . avy-goto-line)
   	 ("M-g w" . avy-goto-word-1) ;; Other options -1 nad -0
   		 )
  :custom (avy-all-windows 'all-frames)
  )

(use-package breadcrumb
  :ensure
  :init
  (breadcrumb-mode))

(use-package sr-speedbar
  :ensure)

(use-package vundo
  :ensure
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package marginalia
  :ensure
  :init
  (marginalia-mode)
  )

(use-package hl-todo
  :ensure
  ;; Highlight comments
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

(use-package rainbow-delimiters
  :ensure
  :hook((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

;; (use-package auto-dark
;;   :diminish
;;   :ensure
;;   :after leuven-theme
;;   :custom
;;   (auto-dark-themes '((leuven-dark) (leuven)))
;;   :config
;;   (auto-dark-mode 1)
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;               (when (display-graphic-p frame)
;;                 (with-selected-frame frame (auto-dark-mode 1))))))

(use-package all-the-icons
  :ensure)

(use-package all-the-icons-completion
  :ensure
  :after all-the-icons
  :init
  (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  )
(use-package all-the-icons-dired
  :ensure
  :after all-the-icons dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil)
  )

(use-package wgrep
  :ensure
  :init
  (setq
   wgrep-auto-save-buffer t
   wgrep-change-readonly-file t))

;; auto save based on event
;; For built-in timer based alternative: (auto-save-visited-mode t)
(use-package super-save
  :diminish
  :ensure
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (save-delete-trailing-whitespace 'except-current-line)
  :config
  (super-save-mode 1))

(use-package dtrt-indent
  :ensure
  :hook (prog-mode . dtrt-indent-mode))

(use-package helpful
  :diminish
  :ensure
  :init (require 'bind-key)
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)
   ([remap describe-symbol]   . helpful-symbol)
   ("C-h C" . #'helpful-command)))

(use-package help-at-pt
  :custom
  (help-at-pt-display-when-idle t) ; Display messages when idle, without prompting
  )

;;; Indication of local VCS changes
(use-package diff-hl
  :ensure
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode)
    	 (magit-post-refresh . diff-hl-magit-post-refresh)
    	 )
  :after magit
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1))



(use-package multiple-cursors
  :ensure
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)       ))


(use-package expand-region
  ;; Expand selection according to scope
  :ensure
  :bind ("C-=" . er/expand-region))


(use-package drag-stuff
  :ensure
  :diminish
  :autoload drag-stuff-define-keys
  :hook ((text-mode prog-mode) . drag-stuff-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; Major mode stuff


(use-package prog-mode
  :hook
  (prog-mode . display-line-numbers-mode) ; Display line numbers only when in programming modes
  )

(use-package gnuplot
  :ensure
  :mode ("\\.plt\\'" . gnuplot-mode)
  )


(use-package markdown-mode
  :ensure
  :after dash
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-map
    	      ("C-c C-e" . markdown-do)))


(use-package conf-mode
  :mode
  (("/.dockerignore\\'" . conf-unix-mode)
   ("/.gitignore\\'" . conf-unix-mode)
   ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package haskell-mode
  :dash "Haskell"
  :ensure
  )

(use-package diff-mode :mode "\\.patch[0-9]*\\'")

(use-package terraform-mode
  :ensure)



(use-package dockerfile-mode :ensure)
(use-package docker-compose-mode :ensure)

(use-package gitlab-ci-mode :ensure)

(use-package ansible :ensure)

(use-package ssh-config-mode :ensure)
(use-package adoc-mode
  :ensure
  )
(use-package go-mode
  :dash (go-ts-mode "Go")
  :ensure  )


(use-package nix-ts-mode
    :ensure
    :mode "\\.nix\\'")

;; (use-package csv-mode :ensure)
(use-package cmake-mode :ensure
  :dash "CMake"
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package mermaid-mode :ensure)
(use-package yaml-mode :ensure)
(use-package protobuf-mode
  :disabled
  :ensure
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))


;; Tooling



(use-package subword
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))


(use-package direnv
  :ensure
  :config
  (direnv-mode)
  (add-to-list 'warning-suppress-types '(direnv))
  )

(use-package magit
  :ensure
  :bind (("C-c g" . magit-status))
   :custom
   (magit-diff-refine-hunk t "Show word-granularity differences within diff hunks")
   (magit-diff-refine-ignore-whitespace t "Ignore whitespace changes in word-granularity differences")
   (magit-diff-hide-trailing-cr-characters t "Hide trailing ^M")
  )

;; Disable due to: https://github.com/dandavison/magit-delta/issues/9
;; (use-package magit-delta
;;   :ensure
;;   :hook (magit-mode . magit-delta-mode))

(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t "Suppress the display of Flymake error counters when there are no errors.")
  )

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gdb-debuginfod-en)
  (gdb-debuginfod-enable-setting t)
  )

;; REVIEW: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dap.el#L36
(use-package dape
  :ensure
  :config
  (dape-breakpoint-global-mode)
  (add-hook 'dape-compile-hook 'kill-buffer)
  :custom
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  ( dape-info-hide-mode-line nil)

  ( dape-inlay-hints t "Showing inlay hints")
  )


(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :init

  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  :custom
  (eglot-report-progress nil "Prevent Eglot minibuffer spam")
  (eglot-extend-to-xref t "Activate Eglot in cross-referenced non-project files")
  (eglot-autoshutdown t "shutdown language server after closing last file")
  ;;  (eglot-confirm-server-initiated-edits nil "allow edits without confirmation")
  )


;; Custom stuff
;; Display ansi colors in buffer
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package vterm
  :ensure)

;; TODO: eglot, flymake, flyspell, cape

;; REVIEW: FIDO, winner-mode, Icomplete

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  )


(use-package consult
  :ensure
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)             
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s f" . consult-find)               
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  ;;  (setq completion-in-region-function #'consult-completion-in-region)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )

(use-package consult-eglot
  :ensure
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))
;; TODO: https://github.com/oantolin/embark/wiki/Additional-Configuration
(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)         
   ("C-;" . embark-dwim)        
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure ; only need to install it, embark loads it after consult if found
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
      	      ("C-c C-o" . embark-export)))

(use-package consult-eglot-embark
  :ensure
  :after (consult eglot embark))

(use-package consult-flyspell
  :ensure
  :after (consult flyspell))

(use-package vertico
  :ensure
  :init
  (vertico-mode))

(use-package orderless
  :ensure
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; REVIEW: completion styles: substring, partial, initials
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))



;; todo: Use consult-ripgrep instead of project-find-regexp in project.el
;; (require 'keymap) ;; keymap-substitute requires emacs version 29.1?
;; (require 'cl-seq)

;; (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)
;; (cl-nsubstitute-if
;;   '(consult-ripgrep "Find regexp")
;;   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
;;   project-switch-commands)

;; NOTE: M-x describe-keymap corfu-map
(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  
  )


;; TODO: https://github.com/minad/consult/wiki#org-clock
;; (defun consult-clock-in ()
;;   "Clock into an Org agenda heading."
;;   (interactive)
;;   (save-window-excursion
;;     (consult-org-agenda)
;;     (org-clock-in)))

;; (consult-customize consult-clock-in
;;                    :prompt "Clock in: "
;;                    :preview-key "M-.")



 (use-package dired
;;   ;; REVIEW(package): diredfl, peep-dired, dired-narrow
;;   :bind (:map dired-mode-map
;;   			  ("C-c C-p" . wdired-change-to-wdired-mode))
   :custom
   (dired-auto-revert-buffer #'dired-buffer-stale-p "Revert the Dired buffer without prompting.")
   (dired-clean-confirm-killing-deleted-buffers nil "Disable the prompt about killing the Dired buffer for a deleted directory.")
   (dired-create-destination-dirs 'ask)
   (dired-dwim-target t "Propose a target for intelligent moving or copying.")
   (dired-filter-verbose nil)
   (dired-free-space nil)
   (dired-listing-switches "-alh --group-directories-first" "In dired, show hidden files and human readable sizes")
   (dired-omit-verbose nil)
   (dired-recursive-copies 'always)
   (dired-recursive-deletes 'top)
   (dired-vc-rename-file t)
;;   (image-dired-thumb-size 150)
    (dired-omit-files
     (concat
      "\\`[.]?#\\|\\`[.][.]?\\'"
      ;; Syncthing
      "\\|^[a-zA-Z0-9]\\.syncthing-enc\\'"
      "\\|^\\.stfolder\\'"
      "\\|^\\.stversions\\'"
      ;; Python
      "\\|^__pycache__\\'"
      ))
   )

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

;; (use-package dired-hacks-utils
;;   :ensure
;;   :after dired)



;; (use-package paren
;;   :custom
;;   (show-paren-mode t "Visualize matching parens")
;;   (show-paren-context-when-offscreen t)
;;   (show-paren-delay 0.1)
;;   (show-paren-highlight-openparen t)
;;   (show-paren-when-point-in-periphery t)
;;   (show-paren-when-point-inside-paren t)
;;   )

(use-package zoxide
  :ensure
  :bind
  ("M-g z" . zoxide-find-file)
  ("M-g M-z" . zoxide-find-file)
  :hook
  (find-file . zoxide-add)
  )
