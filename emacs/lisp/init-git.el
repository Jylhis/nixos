
;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package
 magit
 :ensure
 :init (require 'bind-key)
 :bind (("C-c g" . magit-status))
 :config
 ;; Show word-granularity differences within diff hunks
 (setq magit-diff-refine-hunk t)
 (setq magit-repository-directories
       '( ;; Directory containing project root directories
         ("~/Developer" . 2)
	 )))

(use-package magit-lfs :ensure :after magit)

(use-package
 magit-todos
 :ensure
 :after magit
 :config (magit-todos-mode 1))

;;(use-package treemacs-magit :after (treemacs magit) :ensure)

(use-package git-link :ensure)


(provide 'init-git)
