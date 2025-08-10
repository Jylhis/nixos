;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Early initialization file for Emacs 29+.
;; This file is loaded before package initialization and GUI setup.

;;; Code:

;; Ensure Emacs version is 29.1 or higher
(when (version< emacs-version "29.1")
  (error "This configuration requires Emacs 29.1 or higher"))

;; Disable package.el in favor of Nix package management
(setq package-enable-at-startup nil)

;; Performance optimizations during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      read-process-output-max (* 1024 1024)) ; 1MB

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024) ; 50MB
                  gc-cons-percentage 0.1)))

;; Disable GUI elements early
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Avoid flash of unstyled content
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Set preferred coding system
(prefer-coding-system 'utf-8)

;;; early-init.el ends here