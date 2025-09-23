;;; platforms.el --- Platform-specific configuration loader -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; This module loads platform-specific configurations and handles
;; conditional package loading based on the detected platform.
;; It provides the foundation for supporting Android, macOS, Linux,
;; and Windows with their specific requirements and optimizations.

;;; Code:

(require 'platform)

;;; Android-specific configuration
(platform-when platform-android-p
  ;; Packages that don't work well on Android
  (defvar platform-disabled-packages
    '(vterm pdf-tools exwm magit-delta all-the-icons-dired)
    "List of packages to disable on Android.")

  ;; Android-specific settings
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;; Optimize for touch input and smaller screens
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq scroll-margin 2)
  (setq scroll-conservatively 10000)

  ;; Termux-specific paths - use environment variables when available
  (when (getenv "EXTERNAL_STORAGE")
    (setq org-directory (expand-file-name "Documents/org" (getenv "EXTERNAL_STORAGE"))))

  ;; Simpler modeline for performance
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  mode-line-buffer-identification " "
                  mode-line-position " "
                  (:eval (propertize "%m" 'face 'mode-line-buffer-id))
                  mode-line-end-spaces))

  ;; Android-specific keybindings
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-S-<tab>") (lambda () (interactive) (other-window -1)))

  ;; Hardware key bindings for common Android keyboards
  (when (getenv "TERMUX_VERSION")
    (global-set-key (kbd "<volume-up>") 'scroll-down-command)
    (global-set-key (kbd "<volume-down>") 'scroll-up-command)))

;;; macOS-specific configuration
(platform-when platform-macos-p
  ;; macOS-specific packages
  (defvar platform-preferred-packages
    '(osx-dictionary osx-trash reveal-in-osx-finder)
    "List of packages preferred on macOS.")

  ;; Proper macOS key handling
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier 'none) ; Allow typing special characters

  ;; macOS-specific keybindings
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

  ;; Use macOS trash
  (setq trash-directory "~/.Trash"))

;;; Linux-specific configuration
(platform-when platform-linux-p
  ;; Linux-specific packages
  (defvar platform-preferred-packages
    '(exwm pinentry mu4e)
    "List of packages preferred on Linux.")

  ;; X11 clipboard integration
  ;; (setq select-enable-clipboard t) ; NOTE: `t` should be the default

  ;; Linux-specific settings
  (setq browse-url-browser-function 'browse-url-xdg-open))

;;; Windows-specific configuration
(platform-when platform-windows-p
  ;; Windows-specific packages
  (defvar platform-disabled-packages
    '(vterm magit-delta)
    "List of packages to disable on Windows.")
  
  ;; Windows-specific settings
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'super)
  
  ;; Use Windows-style line endings when appropriate
  (setq-default buffer-file-coding-system 'utf-8-unix))

;;; Platform-aware package management utilities
(defun platform-package-enabled-p (package)
  "Check if PACKAGE should be enabled on current platform."
  (not (memq package (bound-and-true-p platform-disabled-packages))))

(defmacro platform-use-package (name &rest args)
  "Use package NAME with platform-specific conditions."
  (declare (indent 1))
  `(when (platform-package-enabled-p ',name)
     (use-package ,name ,@args)))

;;; Font fallback system
(defun platform-set-font-with-fallback (font-list height)
  "Set font from FONT-LIST with HEIGHT, using first available font."
  (let ((available-font (seq-find (lambda (font)
                                    (find-font (font-spec :name font)))
                                  font-list)))
    (when available-font
      (set-face-attribute 'default nil
                          :font available-font
                          :height height)
      (message "Platform: Using font %s at height %d" available-font height)
      available-font)))

(defun platform-configure-ui ()
  "Apply platform-specific UI configurations."
  (platform-cond
   (platform-android-p
    ;; Larger fonts for touch screens
    (set-face-attribute 'default nil :height 140)
    ;; Disable toolbar and scrollbar to save space
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
   
   (platform-macos-p
    ;; macOS-appropriate fonts with fallbacks
    (platform-set-font-with-fallback 
     '("SF Mono" "Monaco" "Menlo" "DejaVu Sans Mono") 130)
    ;; Native fullscreen
    (setq ns-use-native-fullscreen t))
   
   (platform-linux-p
    ;; Linux font configuration with fallbacks
    (platform-set-font-with-fallback 
     '("JetBrains Mono" "Source Code Pro" "Fira Code" "DejaVu Sans Mono") 110))
   
   (platform-windows-p
    ;; Windows font configuration with fallbacks
    (platform-set-font-with-fallback 
     '("Consolas" "Courier New" "Lucida Console") 110))))

;;; Platform information utilities
(defun platform-show-info ()
  "Display comprehensive platform information."
  (interactive)
  (let ((info (platform-get-info)))
    (with-output-to-temp-buffer "*Platform Info*"
      (princ "=== Platform Information ===\n\n")
      (dolist (item info)
        (princ (format "%-25s: %s\n" (car item) (cdr item))))
      (princ "\n=== Screen Information ===\n")
      (when (display-graphic-p)
        (princ (format "Screen size: %dx%d pixels\n" 
                       (display-pixel-width) (display-pixel-height)))
        (princ (format "Character size: %dx%d\n"
                       (frame-char-width) (frame-char-height))))
      (princ "\n=== Font Information ===\n")
      (let ((font (face-attribute 'default :font)))
        (if font
            (princ (format "Current font: %s\n" font))
          (princ "Font: (default)\n")))
      (when (display-graphic-p)
        (princ (format "Available fonts: %d\n" (length (font-family-list)))))
      (princ "\n=== Environment ===\n")
      (when platform-android-p
        (princ (format "EXTERNAL_STORAGE: %s\n" (or (getenv "EXTERNAL_STORAGE") "not set")))
        (princ (format "TERMUX_VERSION: %s\n" (or (getenv "TERMUX_VERSION") "not set"))))
      (when (boundp 'org-directory)
        (princ (format "org-directory: %s\n" org-directory))))))

;; Global keybinding for platform info
;; (global-set-key (kbd "C-c p i") #'platform-show-info)

;; Apply platform-specific UI configuration
(when (display-graphic-p)
  (platform-configure-ui))

(provide 'platforms)
;;; platforms.el ends here
