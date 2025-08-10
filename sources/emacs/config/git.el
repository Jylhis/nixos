;;; git.el --- Git and version control configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git integration with Magit and related tools.

;;; Code:

(use-package magit
  :bind (("C-c g" . magit-status))
  :custom
  (magit-diff-refine-hunk t "Show word-granularity differences within diff hunks")
  (magit-diff-refine-ignore-whitespace t "Ignore whitespace changes in word-granularity differences")
  (magit-diff-hide-trailing-cr-characters t "Hide trailing ^M"))

(use-package diff-hl
  :ensure
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :after magit
  :config
  (diff-hl-flydiff-mode 1))

(use-package smerge-mode
  :hook (prog-mode . (lambda ()
                      (when (and buffer-file-name (vc-backend buffer-file-name))
                        (smerge-mode 1))))
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

(provide 'git)
;;; git.el ends here