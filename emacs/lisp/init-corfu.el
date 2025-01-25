;;; init-corfu.el
;;; Commentary:
;;; Code:

;;; Pop-up completion
(use-package corfu
  :ensure
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 1)
  :init
  (global-corfu-mode))

(provide 'init-corfu)
;;; init-corfu.el ends here
