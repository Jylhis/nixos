;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/jylhis/nixos
;; Version: 2.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Modern modular Emacs configuration for enhanced development experience.
;; Configuration is split into logical modules in the config/ directory.

;;; Code:

;; Add config directories to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Ensure package system is available
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Store automatic customization options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Load configuration modules
(require 'core)        ; Core Emacs settings and built-ins
(require 'ui)          ; UI and appearance
(require 'completion)  ; Modern completion framework
(require 'programming) ; Programming and development tools
(require 'writing)     ; Org-mode and documentation
(require 'git)         ; Git and version control
(require 'help)        ; Enhanced help system
(require 'ai)          ; AI integrations

(provide 'init)
;;; init.el ends here