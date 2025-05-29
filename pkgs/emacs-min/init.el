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


(use-package simple
  :custom
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate
   #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode")

  (kill-do-not-save-duplicates t "Remove duplicates from the kill ring to reduce clutter")
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

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Automatically refrech buffer if changed on disk")
  (global-auto-revert-non-file-buffers t "Revert also non-file buffers"))

(use-package recentf
  :custom
  (recentf-mode t "Keep track of open files"))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t "Always follow symlinks when opening files"))

;; Show git info in dired
(use-package dired-git-info
  :ensure
  :bind (:map dired-mode-map
			  (")" . dired-git-info-mode)))

(use-package smerge-mode
  :diminish)

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :ensure
  :diminish
  :autoload drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package cursor-sensor
  :hook  (minibuffer-setup . cursor-intangible-mode))

(use-package cc-mode
  :custom
  (c-basic-offset 4))

;; https://emacsredux.com/blog/2023/03/14/avoid-accidentally-minimizing-emacs/
(global-unset-key (kbd "C-z"))

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

(use-package diff-mode :mode "\\.patch[0-9]*\\'")

(use-package cmake-font-lock :ensure
  :after cmake-mode)

;; TODO: https://github.com/redguardtoo/cpputils-cmake
(use-package modern-cpp-font-lock :ensure :hook c++-mode-hook)

(use-package multiple-cursors
  :ensure
  :bind
  (("M-m e" . mc/edit-lines)
   ("M-m s" . mc/mark-next-like-this-symbol)
   ("M-m w" . mc/mark-next-like-this-word))
  :init
  (global-unset-key (kbd "M-m")))

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


(use-package prog-mode
  :hook
  (prog-mode . prettify-symbols-mode)
  (prog-mode . display-line-numbers-mode) ; Display line numbers only when in programming modes
  )


(use-package dash
  :ensure)

(use-package markdown-mode
  :ensure
  :after dash
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

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
  )




;;; LSP Support
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

;; REVIEW: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dap.el#L36
(use-package dape
  :ensure
  :custom
  ;; Info buffers like gud (gdb-mi)
  (dape-buffer-window-arrangement 'gud)
  ( dape-info-hide-mode-line nil)

  ( dape-inlay-hints t "Showing inlay hints")
  )

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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'emacs-init)
;;; init.el ends here
