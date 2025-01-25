;;; init-eglot.el

;;; Commentary:
;;; Code:

;;; LSP Support
(use-package eglot
 :ensure
 :hook (prog-mode . eglot-ensure)
)


(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


; TODO: If eglot
(use-package consult-eglot
  :ensure)

(use-package consult-eglot-embark
  :ensure)



(provide 'init-eglot)
;;; init-eglot.el ends here
