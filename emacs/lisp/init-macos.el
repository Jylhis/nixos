;;; mj-macos --- MacOS specific configs
;;; Commentary:
"MacOS Specifics"
;;; Code:
(when (eq system-type 'darwin)
  ;; both command keys are 'Super'
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'super)
  ;; Option or Alt is naturally 'Meta'
  (setq mac-option-modifier 'meta)
  ;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
  (setq mac-right-option-modifier 'nil)
  ;; Enable transparent title bar on macOS
  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  )

(provide 'init-macos)
;;; mj-macos.el ends here
