;;; init.el --- General emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; LSP -> eglot
;; treesit
;; flymake
;; eldoc
;; xref
;; imenu?? TODO


;;; TODO
;; - This by default with 'enter': https://www.gnu.org/software/emacs/manual/html_node/emacs/Multi_002dLine-Comments.html
;; - highlight parenthesis when either side of the parenthessis. currently highlight only when outside https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; - font-lock config?
;; - eglot-x

;;;; Customizations:
(defcustom mj-setup-elpa nil
  "Setup package repositories Not needed with Nix."
  :group 'mj
  :type 'boolean)
(defcustom mj-debug-run nil
  "Run Emacs with benchmark and 'debug-on-error'."
  :group 'mj
  :type 'boolean)
(defcustom mj-theme-color 'dark
  "Choose theme color."
  :type
'(choice
(const :tag "Light" light)
(const :tag "Dark" dark))
:group 'mj)
;;; Code:

(use-package gcmh :ensure)
(gcmh-mode 1)

;;;; Rest of the code:
(when mj-setup-elpa
 (add-to-list
  'package-archives '("melpa" . "https://melpa.org/packages/") t)

 (add-to-list
  'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/") t)
 (package-initialize))

(when mj-debug-run

 (setq debug-on-error t) ; Produce backtraces when errors occur: can be helpful to diagnose startup issues
 (use-package
  benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)))


;;; Bootstrap
(setq load-prefer-newer t)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))


;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(use-package emacs
 :init
 (tool-bar-mode -1)
 (when scroll-bar-mode (scroll-bar-mode -1))
 (menu-bar-mode -1)

 :custom

 ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  (enable-recursive-minibuffers t) ; Support opening new minibuffers from inside existing minibuffers.
 ;; Hide commands in M-x which do not work in the current mode.  Vertico
 ;; commands are hidden in normal buffers. This setting is useful beyond
 ;; Vertico.
 (read-extended-command-predicate #'command-completion-default-include-p)

 ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
 )

;;(auto-insert-mode)

;; Ignore case in completions
(setq
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
;;(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
;;(setq window-resize-pixelwise nil)

;;(setq resize-mini-windows 'grow-only)

;; refresh a buffer if changed on disk
(global-auto-revert-mode t)
;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)


(setq large-file-warning-threshold 100000000) ; Warn only when opening files bigger than 100MB

(setq-default delete-by-moving-to-trash t) ; Delete by moving to trash in interactive mode


;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)


;;;; Autosave backup and history
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

(setq
 default-directory "~/" ; TODO: Is this needed?
 vc-follow-symlinks t ; always follow symlinks when opening files
 find-file-visit-truename t ; REsolve symlinks

 inhibit-startup-message t ; Disable startup message
 inhibit-splash-screen t ; Disable splash screen
 initial-scratch-message nil ; Disable initial scratch message
 inhibit-startup-screen t ; Disable initial startup screen

 ;custom-safe-themes t ; Mark all custom themes safe TODO: Needed?


 confirm-kill-processes nil ; when quiting emacs, just kill processes
 enable-local-variables t ; ask if local variables are safe once.
 use-short-answers t ; life is too short to type yes or no
 tab-width 4
 )

(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)


;; Autosave stuff to /tmp
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



(save-place-mode t) ; Automatically save your place in files

;; Save history in minibuffer to keep recent commands easily accessible
;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode)

(recentf-mode t) ; Keep track of open files





;;;; GUI/UX

(use-package move-text
;; Move current line or region by olding Meta-Up and Meta-Down
  :ensure
  :init
  (move-text-default-bindings)
  )

(use-package leuven-theme
  ;; Our theme
 :ensure
 :config
 (if (eq mj-theme-color 'light)
     (load-theme 'leuven t)
   (load-theme 'leuven-dark t)))

(use-package
  ;; Keep cursor at the center of the screen
 centered-cursor-mode
 :ensure
 :config
 (global-centered-cursor-mode)
 )


(setq require-final-newline t) ; Add new line at the end of the file

(global-hl-line-mode t) ; highlight the current line

(setq-default cursor-type 'bar)

(global-font-lock-mode 1) ; always highlight code

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; Disable the obsolete practice of end-of-line spacing from the
;; typewriter era.
(setq sentence-end-double-space nil)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;; Smoother and nicer scrolling
(setq
 scroll-margin 0
 scroll-step 1
 next-line-add-newlines nil
 scroll-conservatively 10000
 scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)

;; Reduce cursor lag by :
;; 1. Prevent automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolve the issue of random half-screen jumps during scrolling.
;;(setq auto-window-vscroll nil)

(xterm-mouse-mode 1) ; Enable mouse in terminal

;; Default font to use
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))


;;;;; Modelint
;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;;;; Minibuffer


;;;; Programming

(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; Display line numbers only when in programming modes


(electric-pair-mode t) ; Automatically insert closing parens



(show-paren-mode 1) ; Visualize matching parens
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;;;; C/C++
(setq-default c-basic-offset 4)

;;;;; Web
(use-package web-mode
  :ensure t
  :config
  (setq
   web-mode-enable-auto-closing t
             web-mode-enable-auto-opening t
             web-mode-enable-auto-pairing t
	     web-mode-enable-auto-indentation t
             web-mode-enable-auto-quoting t
	     web-mode-enable-current-column-highlight t
             web-mode-enable-current-element-highlight t
   )
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
   ("\\.html\\'" . web-mode)
   ))


;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)




;;;; Text editing

;; TODO: are both needed?
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing spaces
(autoload 'nuke-trailing-whitespace "whitespace" nil t) ; Remove tailing whitepsace

(use-package adoc-mode :ensure) ; AsciiDoc

;;; Unordered


(use-package vterm :ensure) ; Terminal inside emacs

(setq dired-free-space nil
      dired-dwim-target t  ; Propose a target for intelligent moving or copying.
      dired-deletion-confirmer 'y-or-n-p
      dired-filter-verbose nil
      dired-recursive-deletes 'top
      dired-recursive-copies  'always
      dired-create-destination-dirs 'ask
      ;; Revert the Dired buffer without prompting.
      dired-auto-revert-buffer #'dired-buffer-stale-p
      image-dired-thumb-size 150
      dired-listing-switches "-alh") ; In dired, show hidden files and human readable sizes

(setq dired-vc-rename-file t)

;; Disable the prompt about killing the Dired buffer for a deleted directory.
(setq dired-clean-confirm-killing-deleted-buffers nil)


(setq dired-omit-verbose nil)
(setq dired-omit-files (concat "\\`[.]?#\\|\\`[.][.]?\\'"
                               "\\|\\(?:\\.js\\)?\\.meta\\'"
                               "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                               "\\|^\\.DS_Store\\'"
                               "\\|^\\.\\(?:svn\\|git\\)\\'"
                               "\\|^\\.ccls-cache\\'"
                               "\\|^__pycache__\\'"
                               "\\|^\\.project\\(?:ile\\)?\\'"
                               "\\|^flycheck_.*"
                               "\\|^flymake_.*"))

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
  (when
   (/= arg 1)
   (let ((line-move-visual nil))
     (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point)) (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)


(delete-selection-mode t) ; Delete selected code when typing


;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

;; Define super modifier key. This is sometimes neededn but it's not defined
(define-key function-key-map (kbd "M-]") 'event-apply-super-modifier)


;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;
(use-package treesit
  ;; Builtin tree-sitter
 :ensure nil
 :custom (treesit-font-lock-level 4)
 (major-mode-remap-alist
  '((c-mode . c-ts-mode)
    (c++-mode . c++-ts-mode)
    (go-mode . go-ts-mode)
    (html-mode . html-ts-mde)
    (bash-mode . bash-ts-mode)
    (python-mode . python-ts-mode)
    (typescript-mode . typescript-ts-mode)
    (nix-mode . nix-ts-mode))))


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

;; TODO(v30): This has been integrated with emacs, possibly released with emacs 30
(use-package which-key :ensure :config (which-key-mode))


;; Move between windows with numbers C-x w <num>
(use-package winum :ensure :config (winum-mode))

(use-package terraform-mode :ensure)

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
 (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
 (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
 (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)
 ;;(completion-styles '(basic substring partial-completion flex))
 :init
 (vertico-mode)
 )

;; ;; Use `consult-completion-in-region' if Vertico is enabled.
;; ;; Otherwise use the default `completion--in-region' function.
 (setq completion-in-region-function
       (lambda (&rest args)
         (apply
          (if vertico-mode
              #'consult-completion-in-region
            #'completion--in-region)
          args)))

(use-package orderless
 :ensure
 :config

(defun +orderless--consult-suffix ()
   "Regexp which matches the end of string with Consult tofu support."
   (if (and (boundp 'consult--tofu-char)
            (boundp 'consult--tofu-range))
       (format
        "[%c-%c]*$"
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


 ;; Define orderless style with initialism by default
 (orderless-define-completion-style
  +orderless-with-initialism
  (orderless-matching-styles
   '(orderless-initialism orderless-literal orderless-regexp)))



 (setq
  completion-styles '(substring orderless basic)

  completion-category-defaults nil

  completion-category-overrides
  '((file (styles partial-completion)) ;; partial-completion is tried first
    ;; enable initialism by default for symbols
    (command (styles +orderless-with-initialism))
    (variable (styles +orderless-with-initialism))
    (symbol (styles +orderless-with-initialism))
    (eglot (styles orderless))
    (eglot-capf (styles orderless)))

  orderless-component-separator  #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
  orderless-style-dispatchers (list #'+orderless-consult-dispatch #'orderless-affix-dispatch)
 ))


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
  :init
  (require 'bind-key)
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)
   ("C-h C" . #'helpful-command)))

(use-package breadcrumb
  :ensure
  :init (breadcrumb-mode))

(use-package expand-region
  ;; Expand selection according to scope
  :ensure
  :bind
  ("C-=" . er/expand-region)
  )

(use-package direnv
  ;; TODO: ? envrc https://github.com/purcell/envrc
  :ensure
  :config
  (direnv-mode)
  )

(use-package git-modes
  :ensure
  :config
  (add-to-list 'auto-mode-alist
             (cons "/.dockerignore\\'" 'gitignore-mode))
  )


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
      (when (use-region-p) (deactivate-mark))
      (when (< (car bounds) (point)) (goto-char (car bounds)))
      (consult-line
       (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (t
      (setq isearch-error "No thing at point")
      (consult-line)))))

;; Extended completion utilities
;; https://github.com/minad/consult?tab=readme-ov-file#use-package-example
(use-package consult
 :ensure
 ;; Enable automatic preview at point in the *Completions* buffer. This is
 ;; relevant when you use the default completion UI.
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :bind
 (("C-s" . consult-line)
  ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  ;;("M-#" . consult-register-load)
  ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;("C-M-#" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop) ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  ("M-g e" . consult-compile-error)
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
  ;;("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map
  isearch-mode-map
  ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
  ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  :map
  minibuffer-local-map
  ("M-s" . consult-history) ;; orig. next-matching-history-element
  ("M-r" . consult-history))

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
  xref-show-definitions-function #'consult-xref)
)


;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
 :after vertico
 :ensure
 :init (marginalia-mode)
 )

(use-package embark
 :ensure
 :bind
 (("C-." . embark-act) ;; pick some comfortable binding
  ("C-;" . embark-dwim) ;; good alternative: M-.
  ("C-h B" . embark-bindings))
 ;; alternative for `describe-bindings'
 :init
 ;; Optionally replace the key help with a completing-read interface
 (setq prefix-help-command #'embark-prefix-help-command)

 ;; Show the Embark target at point via Eldoc. You may adjust the
 ;; Eldoc strategy, if you want to see the documentation from
 ;; multiple providers. Beware that using this can be a little
 ;; jarring since the message shown in the minibuffer can be more
 ;; than one line, causing the modeline to move up and down:

 (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)


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
 :hook
 (embark-collect-mode . consult-preview-at-point-mode)
 )


(use-package consult-eglot :ensure)

(use-package consult-eglot-embark :ensure)
(use-package consult-yasnippet :ensure)

(use-package typescript-mode :ensure)


;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless
                 buffer-file-name
                 (let ((buffer-file-name (buffer-name)))
                   (set-auto-mode)))))

;; Icons
(use-package all-the-icons :ensure)
(use-package all-the-icons-dired
 :ensure
 :after dired
 :hook (dired-mode . all-the-icons-dired-mode)
 :config (setq all-the-icons-dired-monochrome nil))


(use-package ace-link
;; TODO: What is this?
  :ensure :config (ace-link-setup-default))


;; Make script file executable by default
(add-hook
 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent :ensure)

;;; Indication of local VCS changes
(use-package diff-hl
 :ensure
 :config (add-hook 'prog-mode-hook #'diff-hl-mode))
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)



(use-package diff-mode :ensure nil :mode "\\.patch[0-9]*\\'")

(use-package cmake-font-lock :ensure)

(use-package cmake-mode
 :ensure
 :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; TODO: Useful?
;;(use-package rtags :ensure)


;; TODO: https://github.com/redguardtoo/cpputils-cmake
(use-package modern-cpp-font-lock :ensure :hook c++-mode-hook)

(use-package google-c-style :ensure)

;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
 :ensure
 :init (require 'bind-key)
 :bind (("C-c g" . magit-status))
 :config
 ;; Show word-granularity differences within diff hunks
 (setq magit-diff-refine-hunk t)
 (setq magit-repository-directories
       '( ;; Directory containing project root directories
         ("~/Developer" . 2))))

(use-package magit-lfs :ensure :after magit)

(use-package magit-todos
 :ensure
 :after magit
 :config (magit-todos-mode 1))

(use-package deadgrep :ensure)

(use-package dash
  ;; https://github.com/magnars/dash.el
  :ensure)

(use-package markdown-mode
  :ensure
  :after dash
:mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  )

(use-package yaml-mode :ensure)

(use-package highlight-indentation :ensure)

;;; Pop-up completion
(use-package corfu
 :ensure
 :custom
 (corfu-auto t)
 (corfu-cycle t)
 (corfu-auto-delay 1)
 :init
 (global-corfu-mode)
 )

(use-package corfu-info
  :ensure nil
  :after corfu)
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook
  (corfu-mode . corfu-popupinfo-mode))
(use-package corfu-echo
  :after corfu
  :ensure nil
  :hook
  (corfu-mode . corfu-echo-mode))

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
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dict)
  ;; Company backends
  ;; (add-hook 'completion-at-point-functions
  ;;              (mapcar #'cape-company-to-capf
  ;;                      (list #'company-files #'company-slime)))

  )


(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

;;; LSP Support
(use-package eglot
  :ensure
  :hook (prog-mode . eglot-ensure)
  )
(setq eglot-report-progress nil)  ; Prevent Eglot minibuffer spam
(setq eglot-extend-to-xref t) ; Activate Eglot in cross-referenced non-project files

;; (use-package copilot
;;  :ensure
;;  :bind
;;  (:map
;;   copilot-completion-map
;;   ("<tab>" . 'copilot-accept-completion)
;;   ("TAB" . 'copilot-accept-completion)
;;   ("C-TAB" . 'copilot-accept-completion-by-word)
;;   ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package eldoc
  :init
  (global-eldoc-mode)
  )



;; Collects and displays all available documentation immediately, even if
;; multiple sources provide it. It concatenates the results.
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
(setq eldoc-echo-area-use-multiline-p t) ; TODO: Move to use-package


(setq help-at-pt-display-when-idle t) ; Display messages when idle, without prompting

;; Enabled inline static analysis
(use-package flymake
  :ensure
  :init
  :hook
  (prog-mode . flymake-mode)
  )
(setq flymake-fringe-indicator-position 'left-fringe)
;; Suppress the display of Flymake error counters when there are no errors.
(setq flymake-suppress-zero-counters t)



(use-package flymake-shellcheck
 :ensure
 :commands flymake-shellcheck-load


 :init (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
 )


(use-package flymake-ruff
 :ensure t
 :hook
 (eglot-managed-mode . flymake-ruff-load)
 ((python-mode python-ts-mode) . flymake-ruff-load)
 )

(use-package flymake-yamllint
  :ensure
  :hook
  ((yaml-mode yaml-ts-mode) . flymake-yamllint-setup))

;; https://github.com/ROCKTAKEY/flymake-elisp-config
(use-package flymake-elisp-config :ensure)
(use-package flymake-ansible-lint :ensure
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode))
  )
(use-package flymake-hadolint :ensure
  :config
  (add-hook 'dockerfile-mode-hook #'flymake-hadolint-setup))
(use-package flymake-json :ensure
  :config
  (add-hook 'json-mode-hook 'flymake-json-load))


(use-package dockerfile-mode :ensure)
(use-package docker-compose-mode :ensure)

(use-package editorconfig
  :ensure
  :config (editorconfig-mode 1))
(use-package editorconfig-generate :ensure)
(use-package editorconfig-domain-specific :ensure)
(use-package editorconfig-custom-majormode :ensure)

(use-package gitlab-ci-mode :ensure)

(use-package avy ; Jump around easily
 :ensure
 :config
 (global-set-key (kbd "C-c z") #'avy-goto-word-1)
 (setq avy-all-windows 'all-frames))

(use-package go-mode
 :ensure
 :bind (:map go-mode-map ("C-c C-f" . 'gofmt))
 ;;:hook (before-save . gofmt-before-save)

 ;; TODO: Use :hook
 :config (add-hook 'go-mode-hook #'yas-minor-mode))

(use-package json-snatcher
  ;; https://github.com/Sterlingg/json-snatcher
  :ensure)

(use-package terraform-mode :ensure)

(use-package json-mode
 :ensure
 :mode "\\.json\\'"
 :after
 (json-snatcher)
 )

(use-package
 nix-ts-mode
 :ensure
 :mode "\\.nix\\'"
 :config
 (add-to-list 'eglot-server-programs '((nix-ts-mode nix-mode) . ("nixd")))
 )



(use-package ruff-format :ensure)

(use-package yasnippet
 :ensure
 :config
 (yas-reload-all)

 (yas-global-mode 1)

 )

(use-package yasnippet-snippets
  :ensure
  :after (yasnippet)
  )

;; (use-package paredit
;;   :ensure t
;;   :hook ((emacs-lisp-mode . enable-paredit-mode)
;;          (lisp-mode . enable-paredit-mode)
;;          (ielm-mode . enable-paredit-mode)
;;          (lisp-interaction-mode . enable-paredit-mode)
;;          (scheme-mode . enable-paredit-mode)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'emacs-init)
;;; emacs-init.el ends here
