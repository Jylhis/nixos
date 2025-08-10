;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Emacs configuration including built-in packages and basic settings.

;;; Code:

(use-package emacs
  :custom
  (text-mode-ispell-word-completion nil "Emacs 30 and newer: Disable Ispell completion function.")
  (context-menu-mode t "Enable context menu for vertico")
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
  (load-prefer-newer t "Always load newer compiled files")
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window)
   ("M-p" . scroll-down-line)
   ("M-n" . scroll-up-line)))

(use-package window
  :custom
  ;; Prefer side by side splitting
  (split-width-threshold 170)
  (split-height-threshold nil))

(use-package files
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom
  ;; Disable autosave and backups
  (auto-save-default nil "Disable separate autosave files")
  (make-backup-files nil "Disable auto backup files")
  (find-file-visit-truename t "Resolve symlinks")
  (confirm-kill-processes nil "when quitting emacs, just kill processes")
  (enable-local-variables t "ask if local variables are safe once"))

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Automatically refresh buffer if changed on disk")
  (global-auto-revert-non-file-buffers t "Revert also non-file buffers"))

(use-package recentf
  :custom
  (recentf-max-saved-items 50 "Increase the default a bit")
  (recentf-mode t "Keep track of open files"))

(use-package savehist
  :init
  (savehist-mode))

(use-package simple
  :custom
  (read-extended-command-predicate
   #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode")
  (kill-do-not-save-duplicates t "Remove duplicates from the kill ring to reduce clutter")
  (line-number-mode t "Show line number in modeline")
  (column-number-mode t "Show column number"))

(use-package subword
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package repeat
  :config
  (repeat-mode))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package dired
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
  (dired-omit-files (concat
                     "\\`[.]?#\\|\\`[.][.]?\\'"
                     "\\|^[a-zA-Z0-9]\\.syncthing-enc\\'"
                     "\\|^\\.stfolder\\'"
                     "\\|^\\.stversions\\'"
                     "\\|^__pycache__\\'")))

(use-package dired-x
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package ibuffer
  :bind
  (([remap list-buffers] . ibuffer)))

(use-package ffap
  :custom
  (ffap-machine-p-known 'reject)) ; Don't attempt to ping unknown hostnames

;;; Essential packages
(use-package super-save
  :ensure
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config
  (super-save-mode 1))

(use-package vundo
  :ensure
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package drag-stuff
  :ensure
  :diminish
  :autoload drag-stuff-define-keys
  :hook ((text-mode prog-mode) . drag-stuff-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

(use-package pretty-sha-path
  :ensure
  :hook ((shell-mode dired-mode) . pretty-sha-path-mode))

;; Startup performance monitoring
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'core)
;;; core.el ends here