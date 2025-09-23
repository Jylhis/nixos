;;; systems.el --- sysadmin and stuff to work with systems -*- lexical-binding: t; -*-

;;; Commentary:
;; logview etc.
;;; Code:

(use-package logview
  :ensure
  :custom
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

(use-package sops
  :ensure t
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :config
  (global-sops-mode 1))

;; FIXME: Maybe not the right place?
(use-package auth-source-1password
  :ensure
  :config
  ;; Customize auth-source-1password-vault to select default vault
  (auth-source-1password-enable)
  )

(provide 'systems)
;;; systems.el ends here
