;;; android.el --- Enhanced Android/Termux configuration -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Enhanced Android-specific configuration based on Termux environment.
;; This module provides Android-optimized keybindings, UI adaptations,
;; and hardware-specific features like volume key bindings and modifier bar.

;;; Code:

(require 'platform)

;; Only load on Android
(platform-when platform-android-p

  ;;; Hardware Integration
  ;; Enable modifier bar for easier key input
  (when (fboundp 'modifier-bar-mode)
    (modifier-bar-mode t))

  ;;; Volume Key Bindings
  ;; Use volume keys as modifier keys for easier typing
  (when (getenv "TERMUX_VERSION")
    ;; Volume-Up as Control modifier
    (define-key key-translation-map
      (kbd "<volume-up>")
      #'tool-bar-event-apply-control-modifier)
    
    ;; Volume-Down as Meta modifier  
    (define-key key-translation-map
      (kbd "<volume-down>")
      #'tool-bar-event-apply-meta-modifier)
    
    ;; Media keys as alternative modifiers (for some keyboards)
    (define-key key-translation-map
      (kbd "<media-previous>")
      #'tool-bar-event-apply-control-modifier)
    
    (define-key key-translation-map
      (kbd "<media-next>")
      #'tool-bar-event-apply-meta-modifier))

  ;;; Custom Menu for Touch Interface
  (when (display-graphic-p)
    (easy-menu-define android-menu global-map
      "Custom menu optimized for Android touch interface."
      '("Android"
        ("File"
         ["Save buffers" save-some-buffers]
         ["Open init file" (lambda () (interactive) 
                             (find-file (expand-file-name "init.el" user-emacs-directory)))]
         ["Recent files" consult-recent-file])
        ("Org"
         ["Agenda" org-agenda]
         ["Capture" org-capture]
         ["TODO List" (lambda () (interactive) (org-agenda nil "t"))]
         ["Search" org-search-view])
        ("Git"
         ["Status" magit-status :enable (fboundp 'magit-status)]
         ["Log" magit-log-current :enable (fboundp 'magit-log-current)])
        ("Navigation"
         ["Switch buffer" consult-buffer]
         ["Find file" find-file]
         ["Search" consult-line]))))

  ;;; Screen Size Adaptations
  (defun android-phone-p ()
    "Return non-nil if running on a phone-sized screen."
    (< (display-pixel-width) 1200))

  (defun android-tablet-p ()
    "Return non-nil if running on a tablet-sized screen."
    (not (android-phone-p)))

  ;;; Dynamic Font Sizing
  (let ((size (if (android-phone-p) 180 130)))
    (set-face-attribute 'default nil :height size))

  ;;; Touch Optimizations
  ;; Enable visual line mode for better text wrapping
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'prog-mode-hook #'visual-line-mode)
  
  ;; Smooth scrolling for touch screens
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  ;;; Storage Integration
  ;; Set up org directory to use external storage
  (let ((external-storage (getenv "EXTERNAL_STORAGE")))
    (when external-storage
      (setq org-directory 
            (expand-file-name "Documents/syncthing" external-storage))
      ;; Alternative path structure for different setups
      (unless (file-directory-p org-directory)
        (setq org-directory
              "/storage/emulated/0/Documents/syncthing"))))

  ;;; Performance Optimizations
  ;; Reduce GC pressure on mobile devices
  (setq gc-cons-threshold (* 20 1024 1024)) ; 20MB
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  ;; Simplify some UI elements for performance
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:propertize mode-line-buffer-identification
                               face mode-line-buffer-id)
                  " " mode-line-position
                  (:eval (when (> (buffer-size) 1000)
                           (format " %dk" (/ (buffer-size) 1000))))
                  " " (:propertize "%m" face mode-line-buffer-id)
                  mode-line-end-spaces))

  ;;; Android-Specific Keybindings
  ;; Easier window navigation for touch screens
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))
  
  ;; Quick access to common functions
  (global-set-key (kbd "C-c C-c") 'save-buffer) ; Easy save
  (global-set-key (kbd "C-c C-k") 'kill-buffer) ; Easy kill
  
  ;; Touch-friendly search
  (global-set-key (kbd "C-c s") 'consult-line)
  (global-set-key (kbd "C-c f") 'consult-find)

  ;;; Package Customizations for Android
  ;; Disable packages that don't work well on Android
  (defvar android-disabled-packages
    '(vterm pdf-tools exwm magit-delta all-the-icons-dired
      lsp-mode eglot company-mode flycheck)
    "Packages to disable on Android for performance/compatibility.")

  ;; Configure remaining packages for Android
  (with-eval-after-load 'org
    ;; Simpler org setup for mobile
    (setq org-startup-folded 'content)
    (setq org-image-actual-width '(400)) ; Smaller images
    (setq org-hide-emphasis-markers t))

  (with-eval-after-load 'magit
    ;; Simpler magit interface
    (setq magit-diff-refine-hunk nil) ; Disable for performance
    (setq magit-revision-show-gravatars nil))

  ;;; Helper Functions
  (defun android-restart-emacs ()
    "Restart Emacs on Android/Termux."
    (interactive)
    (save-some-buffers)
    (kill-emacs)
    ;; Termux will restart Emacs automatically
    )

  (defun android-show-platform-info ()
    "Display Android platform information."
    (interactive)
    (let ((info (platform-get-info)))
      (with-output-to-temp-buffer "*Android Platform Info*"
        (princ "Android/Termux Platform Information:\n\n")
        (dolist (item info)
          (princ (format "%-20s: %s\n" (car item) (cdr item))))
        (princ (format "\nScreen size: %dx%d (phone: %s)\n"
                       (display-pixel-width) (display-pixel-height)
                       (if (android-phone-p) "yes" "no")))
        (princ (format "External storage: %s\n" 
                       (or (getenv "EXTERNAL_STORAGE") "not found")))
        (princ (format "Org directory: %s\n" org-directory)))))

  ) ; End platform-when

(provide 'android)
;;; android.el ends here