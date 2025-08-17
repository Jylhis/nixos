;;; utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom utility functions and helpers.

;;; Code:

(require 'ansi-color)

(defun display-ansi-colors ()
  "Apply ANSI color codes to the current buffer.
This function processes the entire buffer and interprets any ANSI
escape sequences, rendering the corresponding colors in the buffer.

Useful for viewing logs or other text files that include ANSI
color codes."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Org-mode utilities
(defun my/find-org-files-recursively (directory)
  "Find all .org files recursively in DIRECTORY, ignoring hidden folders."
  (when (and directory (file-exists-p directory) (file-directory-p directory))
    (let ((files '()))
      (dolist (file (directory-files-recursively directory "\\.org\\'" t
                                                 (lambda (dir)
                                                   (not (string-match-p "\\(^\\|/\\)\\." (file-name-nondirectory dir))))))
        (when (file-regular-p file)
          (push (file-truename file) files)))
      (nreverse files))))

(defun my/update-org-agenda-files ()
  "Update org-agenda-files to include all .org files under Documents."
  (let ((org-files (my/find-org-files-recursively (expand-file-name org-directory))))
    (when org-files
      (setq org-agenda-files org-files))
    (message "Updated org-agenda-files: %d files found" (length org-files))))

(defun my/setup-org-agenda-files ()
  "Set up dynamic org agenda files updating."
  ;; Initial update
  (my/update-org-agenda-files)
  
  ;; Update agenda files periodically instead of on every agenda access
  (run-with-idle-timer 300 t #'my/update-org-agenda-files))

(provide 'utils)
;;; utils.el ends here