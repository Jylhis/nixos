;;; init.el --- General emacs config. -*- lexical-binding: t -*-
;;; Commentary:
;; see README.adoc for docs

;;; Code:

;;(setq debug-on-error t) ; Produce backtraces when errors occur: can be helpful to diagnose startup issues

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb

;; Don't ping things that look like domain names.
(use-package ffap
  :custom
  (ffap-machine-p-known 'reject))

;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)

(use-package diminish :ensure)

;; Garbage Collector Magic Hack
(use-package gcmh
  :ensure
  :diminish
  :init
  (gcmh-mode t)
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package emacs
  :init
  (tool-bar-mode -1)
  (when scroll-bar-mode
	(scroll-bar-mode -1))
  (menu-bar-mode -1)

  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  (enable-recursive-minibuffers t "Support opening new minibuffers from inside existing minibuffers")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt) "Do not allow the cursor in the minibuffer prompt")
  (read-buffer-completion-ignore-case t "Ignore case when reading buffer name")
  (completion-ignore-case t "Don't condifer case significant in completion")

  (delete-by-moving-to-trash t "Delete by moving to trahs in interactive mode")

  (sentence-end-double-space nil "Disable the obsolete practice of end-of-line spacing from the typewriter era.")


  ;; NOTE: the inhibit-startup-message Emacs option
  ;; cannot be set here since Emacs disallows setting it from the default
  ;; initialization file.
  (inhibit-startup-message t "Disable startup message")
  (inhibit-splash-screen t "Disable splash screen")
  (initial-scratch-message nil "Disable initial scratch message")
  (inhibit-startup-screen t "Disable initial startup screen")

  ;; (set-mark-command-repeat-pop t "Repeating C-SPC after popping mark pops it again")

  (use-short-answers t "life is too short to type yes or no")
  (tab-width 4)
  (save-place-mode t "Automatically save your place in files")

  (cursor-type 'bar)

  (word-wrap t "Continue wrapped lines at whitespace rather than breaking in the middle of a word.")

  (visible-bell nil "No blinking")
  (ring-bell-function #'ignore "No beeping")

  (sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ; REVIEW
  (sentence-end-double-space nil); REVIEW


  (scroll-margin 15 "Keep 15 line margin from top and bottom")

  (scroll-conservatively 10000)
  (scroll-preserve-screen-position 1 "keep the cursor in the same position while scrolling"))

(use-package super-save
  :ensure t
  :custom
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :hook (after-init . savehist-mode))

(setopt comment-multi-line t)

(use-package newcomment
  :custom
  (comment-multi-line t "Enable multi-line commenting which ensures that")
  (comment-empty-lines t "Ensures that empty lines within the commented region are also commented out"))

(use-package elec-pair
  :diminish
  :hook (after-init . electric-pair-mode))

(use-package subword
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Handle minified code
(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package files
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save . executable-make-buffer-file-executable-if-script-p) ; Make script file executable by default
  :custom
  (require-final-newline t "Add new line at the end of the file")
  (find-file-visit-truename t "Resolve symlinks")
  (confirm-kill-processes nil "when quitting emacs, just kill processes")
  (enable-local-variables t "ask if local variables are safe once")
  :config
  ;; Disable autosave and backups
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  )

(use-package custom
  :custom
  (custom-safe-themes t "Mark all custom themes safe"))

(use-package font-core
  :custom
  (global-font-lock-mode 1 "always highlight code"))

(use-package paren
  :custom
  (show-paren-mode t "Visualize matching parens")
  (show-paren-context-when-offscreen t)
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  )

(use-package dired
  :bind (:map dired-mode-map
			  ("C-c C-p" . wdired-change-to-wdired-mode))
  :custom
  (dired-free-space nil)
  (dired-dwim-target t "Propose a target for intelligent moving or copying.")
  ;;(dired-deletion-confirmer 'y-or-n-p)
  (dired-filter-verbose nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-create-destination-dirs 'ask)
  (dired-auto-revert-buffer #'dired-buffer-stale-p "Revert the Dired buffer without prompting.")
  (image-dired-thumb-size 150)
  (dired-listing-switches "-alh --group-directories-first" "In dired, show hidden files and human readable sizes")
  (dired-vc-rename-file t)
  (dired-clean-confirm-killing-deleted-buffers nil "Disable the prompt about killing the Dired buffer for a deleted directory.")
  (dired-omit-verbose nil)
  (dired-omit-files
   (concat
    "\\`[.]?#\\|\\`[.][.]?\\'"
    "\\|\\(?:\\.js\\)?\\.meta\\'"
    "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
    "\\|^\\.DS_Store\\'"
    "\\|^\\.\\(?:svn\\|git\\)\\'"
    "\\|^\\.ccls-cache\\'"
    "\\|^__pycache__\\'"
    "\\|^\\.project\\(?:ile\\)?\\'"
    "\\|^flycheck_.*"
    "\\|^flymake_.*"))
  )

(use-package dired-x
  :after dired)

(use-package dired-hacks-utils
  :ensure
  :after dired)

(use-package text-mode
  :custom
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil))

(use-package simple
  :custom
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate
   #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode")

  (kill-do-not-save-duplicates t "Remove duplicates from the kill ring to reduce clutter")
  (next-line-add-newlines nil)
  (line-number-mode t "Show line number in modeline")
  (column-number-mode t "Show column number")
  )

(use-package mwheel
  :custom
  (mouse-wheel-follow-mouse t "Scroll the windows that mouse is over"))

(use-package xt-mouse
  :custom
  (xterm-mouse-mode 1 "Enable mouse in terminal"))

(use-package minibuffer
  :custom
  (read-file-name-completion-ignore-case t "ignore case when reading file name"))

(global-set-key (kbd "C-<tab>") 'completion-at-point) ;; TODO: Move inside of the use-package

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Automatically refrech buffer if changed on disk")
  (global-auto-revert-non-file-buffers t "Revert also non-file buffers"))

;; (use-package mouse
;;   :custom
;;   (mouse-yank-at-point t))

(use-package recentf
  :custom
  (recentf-mode t "Keep track of open files"))

(use-package window
  :custom
  ;; Prefer side by side splitting
  (split-width-threshold 170)
  (split-height-threshold nil))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t "Always follow symlinks when opening files"))

;; Show git info in dired
(use-package dired-git-info
  :ensure
  :bind (:map dired-mode-map
			  (")" . dired-git-info-mode)))

;; `find-dired' alternative using `fd'
(use-package fd-dired
  :ensure)

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode org-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package smerge-mode
  :diminish)

(use-package gnuplot
  :ensure)

(use-package org-appear
  :ensure
    :hook
    (org-mode . org-appear-mode)
    :after org)

(use-package olivetti
  :ensure
  :hook
  (org-mode . olivetti-mode))

(use-package org-modern
  :ensure
  :after org
    :hook
    (org-mode . global-org-modern-mode)
    :custom
    (org-modern-keyword nil)
    (org-modern-checkbox nil)
    (org-modern-table nil))

;; TODO: Define org-agenda-files
(use-package org
  :custom
  (org-hide-emphasis-markers t "Hide / around italics")
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-directory "~/Documents")
  (org-default-notes-file (concat org-directory "/notes.org"))
  :config
  (setq org-agenda-files (mapcar 'file-truename (file-expand-wildcards (concat org-directory "/**/*.org"))))
  (setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
   (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  :hook
  (org-mode . visual-line-mode) ; variable-pitch-mode
  :bind (
		 ("C-c a" . org-agenda)
		 ("C-c c" . org-capture)))

(use-package org-bullets
  :ensure
  :after org
  :init
  (setq org-bullets-bullet-list
        '("●"))
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :ensure
  :diminish
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))


(use-package ox-man
  :after org-mode)

(use-package htmlize
  :ensure
  :after org-mode)

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package cursor-sensor
  :hook  (minibuffer-setup . cursor-intangible-mode))

(use-package cc-mode
  :custom
  (c-basic-offset 4))

(use-package
  leuven-theme
  :ensure
  :config
  (load-theme 'leuven t t)
  (load-theme 'leuven-dark t t))

(use-package auto-dark
  :ensure
  :after leuven-theme
  :custom
  (auto-dark-themes '((leuven-dark) (leuven)))
  :init (auto-dark-mode))

;; Ugly hack for emacs server FIXME: implement this
(defun id/server-auto-dark (frame)
  (use-package auto-dark
    :ensure
    :config (auto-dark-mode t)
    :diminish auto-dark-mode
    :custom
    (auto-dark-dark-theme 'leuven-dark)
    (auto-dark-light-theme 'leuven)
    )
  (remove-hook 'after-make-frame-functions #'id/server-auto-dark)
  )
(add-hook 'after-make-frame-functions #'id/server-auto-dark)


;; GUI Specific configs
;; (add-hook 'after-make-frame-functions
;;   (lambda ()
;;     ;; we want some font only in GUI Emacs
;;     (when (display-graphics-p)
;;
;; 	  ))
;;
;; ;; Terminal specific configs
;; (add-hook 'after-make-frame-functions
;;   (lambda ()
;;     ;; we do something only in terminal Emacs
;;     (unless (display-graphics-p)
;;
;; 	  ))

(use-package
  centered-cursor-mode
  :ensure
  :disabled
  :config
  (global-centered-cursor-mode))


;; REVIEW
;; Default font to use
(defun font-available-p (font-name)
  (find-font (font-spec :name font-name)))

(cond
 ((font-available-p "Source Code Pro")
  (set-frame-font "Source Code Pro 12")))

;;(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

;; https://emacsredux.com/blog/2023/03/14/avoid-accidentally-minimizing-emacs/
(global-unset-key (kbd "C-z"))

;;;;; Web
(use-package
  web-mode
  :ensure t
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-opening t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.html\\'" . web-mode)))

(use-package adoc-mode :ensure) ; AsciiDoc

(use-package vterm :ensure) ; Terminal inside emacs

(use-package rainbow-delimiters
  :ensure
  :hook((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))


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

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line) ; NOTE: Use M-a to move beginning of the line
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)


;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

;; Define super modifier key. This is sometimes neededn but it's not defined
(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)

(use-package treesit
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '(
     ((js-mode javascript-mode) . js-ts-mode)
     (bash-mode . bash-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-mode . c-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (css-mode . css-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (go-mode . go-ts-mode)
     (html-mode . html-ts-mde)
     (json-mode . json-ts-mode)
     (nix-mode . nix-ts-mode)
     (python-mode . python-ts-mode)
     (sh-mode . bash-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode)
     )))

;; Code folding
(use-package treesit-fold
  :ensure
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))


(use-package hl-todo
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

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  ;;(which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c c" "consult")
  (which-key-add-key-based-replacements "C-c d" "dict")
  (which-key-add-key-based-replacements "C-c l" "link-hint")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x c" "colorful")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x r" "rect & bookmark")
  (which-key-add-key-based-replacements "C-x t" "tab & treemacs")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x X" "edebug")

  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c \"" "org-plot")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-v" "org-babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-x" "org-misc")

  (which-key-add-major-mode-key-based-replacements 'emacs-lisp-mode
    "C-c ," "overseer")
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
    "C-c C-x" "markdown-toggle")

  )


(use-package terraform-mode
  :ensure
  :custom
  (terraform-format-on-save t))

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :ensure
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :init (vertico-mode))

;; ;; Use `consult-completion-in-region' if Vertico is enabled.
;; ;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package orderless
  :ensure
  :custom
  (  completion-styles '(substring orderless basic))
  ;;(  completion-category-defaults nil)
  (  completion-category-overrides
	 '((file (styles partial-completion)) ;; partial-completion is tried first
       ;; enable initialism by default for symbols
       (command (styles +orderless-with-initialism))
       (variable (styles +orderless-with-initialism))
       (symbol (styles +orderless-with-initialism))
       (eglot (styles orderless))
       (eglot-capf (styles orderless))))
  (  orderless-component-separator #'orderless-escapable-split-on-space "allow escaping space with backslash!")
  (  orderless-style-dispatchers (list #'+orderless-consult-dispatch #'orderless-affix-dispatch))
  :config
  (defun +orderless--consult-suffix ()
	"Regexp which matches the end of string with Consult tofu support."
	(if (and (boundp 'consult--tofu-char)
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
		.
		,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp
		.
		,(concat
          "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;;FIXME

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style
	  +orderless-with-initialism
	(orderless-matching-styles
	 '(orderless-initialism orderless-literal orderless-regexp))))



(defun consult--orderless-regexp-compiler (input type &rest _config)
  (setq input (cdr (orderless-compile input)))
  (cons
   (mapcar (lambda (r) (consult--convert-regexp r type)) input)
   (lambda (str) (orderless--highlight input t str))))

(setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

;; Adds intellisense-style code completion at point that works great
;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
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

(use-package vundo
  :ensure
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;; Goto last change
(use-package goto-chg
  :ensure
  :bind ("C-," . goto-last-change))

(use-package breadcrumb
  :ensure
  :init
  (breadcrumb-mode))

(use-package expand-region
  ;; Expand selection according to scope
  :ensure
  :bind ("C-=" . er/expand-region))

(use-package easy-kill :ensure
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package
  direnv
  :ensure
  :config
  (direnv-mode)
  (add-to-list 'warning-suppress-types '(direnv))
  )

(use-package git-modes
  :ensure
  :mode
  ("/.dockerignore\\'" . gitignore-mode))


(defun dn--consult-line-thing-at-point ()
  "Do incremental search forward for the \"thing\" found near point.
Like ordinary incremental search except that the \"thing\" found at point
is added to the search string initially.  The \"thing\" is defined by
`bounds-of-thing-at-point'.  You can customize the variable
`isearch-forward-thing-at-point' to define a list of symbols to try
to find a \"thing\" at point.  For example, when the list contains
the symbol `region' and the region is active, then text from the
active region is added to the search string."
  (interactive)
  (let ((bounds
         (seq-some
          (lambda (thing)
            (bounds-of-thing-at-point thing))
          isearch-forward-thing-at-point)))
    (cond
     (bounds
      (when (use-region-p)
        (deactivate-mark))
      (when (< (car bounds) (point))
        (goto-char (car bounds)))
      (consult-line
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No thing at point")
      (consult-line)))))

;; Extended completion utilities
;; https://github.com/minad/consult?tab=readme-ov-file#use-package-example
(use-package consult
  :ensure
  :commands consult-customize
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ;; NOTE: What is the diffence between bind and the remap thing
  (([remap isearch-forward]    . consult-line)
   ([remap Info-search] . consult-info)
   ([remap recentf-open-files] . consult-recent-file)

   ;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)

   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer

   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)

   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop

   ;; M-g bindings in `goto-map'
   ;;  ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line) ;; orig. goto-line
   ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   ;; M-s bindings in `search-map'
   ("M-s ." . dn--consult-line-thing-at-point)
   ("M-s d" . consult-fd) ;; Alternative: consult-fd
   ("M-s f" . consult-find)
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s L"   . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
   ("M-s l" . consult-line) ;; needed by consult-line to detect isearch # FIXME: not defined
   ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch

   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history) ;; orig. next-matching-history-element
   ("M-r" . consult-history) ;; orig. previous-matching-history-element
   )

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key
   '(:debounce 0.2 any)
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-file-register
   consult--source-recent-file
   consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  (setq
   read-buffer-completion-ignore-case t
   read-file-name-completion-ignore-case t
   completion-ignore-case t)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq
   register-preview-delay 0.5
   register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref))


;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :ensure
  :hook (after-init . marginalia-mode))

(use-package wgrep
  :ensure
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act) ;; pick some comfortable binding
   ("C-;" . embark-dwim) ;; good alternative: M-.
   ([remap describe-bindings] . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list
   'display-buffer-alist
   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
			  ("C-c C-o" . embark-export)))

(use-package consult-eglot
  :ensure
  :after consult eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :ensure
  :after consult-eglot
  :config
  (consult-eglot-embark-mode))

(use-package flyspell
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package consult-flyspell
  :ensure
  :bind ("M-g s" . consult-flyspell))

(use-package typescript-mode :ensure)

;; Icons
(use-package all-the-icons :ensure)
(use-package all-the-icons-dired
  :ensure
  :after all-the-icons dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom (all-the-icons-dired-monochrome nil)
  )

;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent
  :ensure
  :hook (prog-mode . dtrt-indent-mode))

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


(use-package haskell-mode
  :ensure)

(use-package diff-mode :mode "\\.patch[0-9]*\\'")

(use-package cmake-font-lock :ensure
  :after cmake-mode)

(use-package cheatsheet :ensure
  :config
  (cheatsheet-add-group 'Common
						'(:key "C-x C-c" :description "leave Emacs")
						'(:key "C-x C-f" :description "find file"))
  (cheatsheet-add-group 'Editing
						'(:key "M-w" :description "Easy kill")
						'(:key "C-=" :description "Expand region")
						'(:key "C-x C-;" :description "Comment line")
						'(:key "C-j" :description "New line and maybe indent")
						'(:key "M-j" :description "Default newline (supports multiline comments)")

						)
  (cheatsheet-add-group 'Groups
						'(:key "M-g" :description "Goto")
						'(:key "M-s" :description "Search")
						'(:key "C-x p" :description "Project stuff")
						'(:key "C-x v" :description "Version control")
						'(:key "C-x w" :description "Window")
						'(:key "C-x x" :description "buffer")
						)
  (cheatsheet-add-group 'Modifiers
						'(:key "C-x 4" :description "For other window")
						'(:key "C-x 5" :description "For other frame")
						)
  )

;; TODO: https://github.com/redguardtoo/cpputils-cmake
(use-package modern-cpp-font-lock :ensure :hook c++-mode-hook)

(use-package google-c-style :ensure
  :hook (c-mode-common-hook . google-set-c-style))


(use-package tempel
  :ensure
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  )

(use-package tempel-collection :ensure)

(use-package eglot-tempel
  :ensure
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

(use-package multiple-cursors
  :ensure
  :bind
  (("M-m e" . mc/edit-lines)
   ("M-m s" . mc/mark-next-like-this-symbol)
   ("M-m w" . mc/mark-next-like-this-word))
  :init
  (global-unset-key (kbd "M-m")))

;; (use-package difftastic
;;   :ensure
;;   :defer)
;;
;; (use-package difftastic-bindings
;;   :ensure difftastic ;; or nil if you prefer manual installation
;;   :config (difftastic-bindings-mode))

(use-package magit-delta
  :ensure
  :hook (magit-mode . magit-delta-mode))

;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
  :ensure
  :init (require 'bind-key)
  :bind (("C-c g" . magit-status))
  :custom
  (magit-diff-refine-hunk t "Show word-granularity differences within diff hunks")
  (magit-diff-refine-ignore-whitespace t "Ignore whitespace changes in word-granularity differences")
  (magit-diff-hide-trailing-cr-characters t "Hide trailing ^M")
  (magit-repository-directories
   '( ;; Directory containing project root directories
     ("~/Developer" . 2)))
  )
(use-package magit-lfs :ensure :after magit)

(use-package project
  :custom
  (project-mode-line t))

(use-package git-timemachine :ensure)

(use-package git-messenger :ensure
  :custom
  (git-messenger:use-magit-popup t))


(use-package prog-mode
  :hook
  (prog-mode . prettify-symbols-mode)
  (prog-mode . display-line-numbers-mode) ; Display line numbers only when in programming modes
  )

(use-package magit-todos
  :ensure
  :after magit
  :config (magit-todos-mode t))

(use-package deadgrep :ensure)

(use-package dash
  :ensure)

(use-package hledger-mode
  :ensure
  :after htmlize
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  )

(use-package markdown-mode
  :ensure
  :after dash
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

;; Display ansi colors in buffer
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package highlight-indentation :ensure)

;;; Pop-up completion
(use-package corfu
  :ensure
  :custom
  (corfu-auto t "Enable auto completion")
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-quit-no-match 'separator "configure quitting")
  (corfu-auto-prefix 3)
  (corfu-preview-current nil)
  (corfu-auto-delay 1.0)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode) ;  Display candidate documentation or source in a popup next to the candidate menu.
  (corfu-history-mode) ; remembers selected candidates and sorts the candidates by their history position and frequency.
  (corfu-echo-mode) ; displays a brief candidate documentation in the echo area.
  :config
  (keymap-unset corfu-map "RET") ; Free the RET key for less intrusive behavior.
  )

;; (use-package corfu-candidate-overlay
;;   :ensure
;;   :after corfu
;;   :config
;;   ;; enable corfu-candidate-overlay mode globally
;;   ;; this relies on having corfu-auto set to nil
;;   (corfu-candidate-overlay-mode +1)
;;   ;; bind Ctrl + TAB to trigger the completion popup of corfu
;;   ;;(global-set-key (kbd "C-<tab>") 'completion-at-point)
;;   ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;;   ;; (keybing <iso-lefttab> may not work for your keyboard model)
;;   (global-set-key (kbd "C-<tab>") 'corfu-candidate-overlay-complete-at-point))


;; Add extensions
(use-package cape
  :ensure
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map)
  ;; Alternative keys: M-p, M-+, ...
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
  ;; REVIEW
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;; Company backends
  ;; (add-hook 'completion-at-point-functions
  ;;              (mapcar #'cape-company-to-capf
  ;;                      (list #'company-files #'company-slime)))


  )


;; TODO: https://github.com/minad/corfu/wiki
;; (defun my/eglot-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-capf-super
;;                      #'eglot-completion-at-point
;;                      #'tempel-expand
;;                      #'cape-file))))
;;
;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)



;;; LSP Support
;; tag::eglot-config[]
(use-package eglot
  :ensure
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :custom
  (eglot-report-progress nil "Prevent Eglot minibuffer spam")
  (eglot-extend-to-xref t "Activate Eglot in cross-referenced non-project files")
  (eglot-autoshutdown t "shutdown language server after closing last file")
  (eglot-confirm-server-initiated-edits nil "allow edits without confirmation")
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  :config
  (setq-default eglot-workspace-configuration
				'(:nil
                  (:formatting
                   (:command ["nixfmt"])
                   :nix (:flake (:autoArchive t :autoEvalInputs t)))
                  :gopls (:usePlaceholders t :gofumpt t)
                  :nixd (:formatting (:command ["nixfmt"])))))

;; End::eglot-config[]


;; (use-package copilot
;;  :ensure
;;  :bind
;;  (:map
;;   copilot-completion-map
;;   ("<tab>" . 'copilot-accept-completion)
;;   ("TAB" . 'copilot-accept-completion)
;;   ("C-TAB" . 'copilot-accept-completion-by-word)
;;   ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; REVIEW: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dap.el#L36
(use-package dape
  :ensure
  :custom
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  ( dape-info-hide-mode-line nil)

  ( dape-inlay-hints t "Showing inlay hints")
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))

;; eldoc-box?
(use-package eldoc
  :diminish
  :custom
  ;; Collects and displays all available documentation immediately, even if
  ;; multiple sources provide it. It concatenates the results.
  (eldoc-documentation-strategy
   'eldoc-documentation-compose-eagerly)
  ;;(setq eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-echo-area-use-multiline-p t)

  :init (global-eldoc-mode)
  )

(use-package xref

  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  )

(setq help-at-pt-display-when-idle t) ; Display messages when idle, without prompting


(use-package devdocs
  :ensure
  :commands devdocs-install devdocs--available-docs
  :init
  (defconst devdocs-major-mode-docs-alist
    '((c-mode          . ("c"))
      (c++-mode        . ("cpp"))
      (python-mode     . ("python~3.12" "python~2.7"))
      (ruby-mode       . ("ruby~3.1"))

      (rustic-mode     . ("rust"))
      (css-mode        . ("css"))
      (html-mode       . ("html"))
      (julia-mode      . ("julia~1.8"))
      (js-mode         . ("javascript" "jquery"))
      (emacs-lisp-mode . ("elisp")))
	"Alist of major-mode and docs.")
  (mapc
   (lambda (mode)
     (add-hook (intern (format "%s-hook" (car mode)))
               (lambda ()
                 (setq-local devdocs-current-docs (cdr mode)))))
   devdocs-major-mode-docs-alist)

  (setq devdocs-data-dir (expand-file-name "devdocs" user-emacs-directory))
  (defun devdocs-dwim()
    "Look up a DevDocs documentation entry.

Install the doc if it's not installed."
    (interactive)
    ;; Install the doc if it's not installed
    (mapc
     (lambda (slug)
       (unless (member slug (let ((default-directory devdocs-data-dir))
                              (seq-filter #'file-directory-p
                                          (when (file-directory-p devdocs-data-dir)
                                            (directory-files "." nil "^[^.]")))))
         (mapc
          (lambda (doc)
            (when (string= (alist-get 'slug doc) slug)
              (devdocs-install doc)))
          (devdocs--available-docs))))
     (alist-get major-mode devdocs-major-mode-docs-alist))

    ;; Lookup the symbol at point
    (devdocs-lookup nil (thing-at-point 'symbol t)))
  )

;; Enabled inline static analysis
(use-package flymake
  :ensure
  :diminish
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t "Suppress the display of Flymake error counters when there are no errors.")
  )



(use-package
  flymake-shellcheck
  :ensure
  :commands flymake-shellcheck-load
  :init (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  )


(use-package
  flymake-ruff
  :ensure t
  :hook
  (eglot-managed-mode . flymake-ruff-load)
  ((python-mode python-ts-mode) . flymake-ruff-load))

(use-package
  flymake-yamllint
  :ensure
  :hook ((yaml-mode yaml-ts-mode) . flymake-yamllint-setup))

;; https://github.com/ROCKTAKEY/flymake-elisp-config
(use-package flymake-elisp-config
  :ensure)
(use-package
  flymake-ansible-lint
  :ensure
  :commands flymake-ansible-lint-setup
  :hook
  (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
   ;;((yaml-ts-mode yaml-mode) . flymake-mode)
   ))

(use-package
  flymake-hadolint
  :ensure
  :config
  (add-hook 'dockerfile-mode-hook #'flymake-hadolint-setup))

(use-package
  flymake-json
  :ensure
  :config (add-hook 'json-mode-hook 'flymake-json-load))


(use-package dockerfile-mode :ensure)
(use-package docker-compose-mode :ensure)

(use-package editorconfig
  :ensure
  :mode (".editorconfig" . editorconfig-mode)
  :diminish
)

(use-package gitlab-ci-mode :ensure)

(use-package ansible :ensure)

(use-package ssh-config-mode :ensure)

(use-package avy ; Jump to things in Emacs tree-style
  :ensure
  :bind (
		 ("C-:"   . avy-goto-char)
		 ("C-'"   . avy-goto-char-2)
		 ("M-g l" . avy-goto-line)
		 ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
		 )
  :custom (avy-all-windows 'all-frames)
  )

(use-package go-mode
  :ensure
  :hook
  (go-mode . yas-minor-mode)
  )

(use-package
  json-snatcher
  ;; https://github.com/Sterlingg/json-snatcher
  :ensure)

(use-package terraform-mode :ensure)

(use-package
  json-mode
  :ensure
  :mode "\\.json\\'"
  :after (json-snatcher))

(use-package
  nix-ts-mode
  :ensure
  :mode "\\.nix\\'"
  :config
  (add-to-list
   'eglot-server-programs '((nix-ts-mode nix-mode) "nixd")))


(use-package ruff-format :ensure)


;; Misc programming modes
(use-package csv-mode :ensure)
(use-package cmake-mode :ensure
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package mermaid-mode :ensure)
(use-package yaml-mode :ensure)
(use-package protobuf-mode
  :ensure
  :hook (protobuf-mode . (lambda ()
                           (setq imenu-generic-expression
                                 '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)" 2))))))


;; TODO: use-package calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'emacs-init)
;;; init.el ends here
