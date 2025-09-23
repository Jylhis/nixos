;;; git.el --- Git and version control configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git integration with Magit and related tools.

;;; Code:

(use-package magit
  :bind (("C-c g" . magit-status))
  :custom
  (magit-diff-refine-hunk t "Show word-granularity differences within diff hunks")
  (magit-diff-refine-ignore-whitespace t "Ignore whitespace changes in word-granularity differences")
  (magit-diff-hide-trailing-cr-characters t "Hide trailing ^M")
  (magit-diff-context-lines 5 "Show more context lines for better understanding")
  )

(use-package
 magit-todos
 :ensure
 :after magit
 :config (magit-todos-mode 1))


(use-package diff-hl
  :ensure
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
   (diff-hl-side 'left)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :after magit
  :config
  (diff-hl-flydiff-mode 1))

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  ;; Enhanced three-way merge configuration
  (ediff-diff-options "-w")
  (ediff-custom-diff-options "-u")
  (ediff-merge-revisions-with-ancestor t)
  :config
    (setq ediff-control-frame-parameters
          '((name . "Ediff Control")
            (width . 60)
            (height . 14)
            (left . 200)
            (top . 200)
            (minibuffer . nil)
            (user-position . t)
            (vertical-scroll-bars . nil)
            (scrollbar-width . 0)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0))))



(provide 'git)
;;; git.el ends here
