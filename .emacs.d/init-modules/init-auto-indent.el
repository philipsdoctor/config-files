;;; init-auto-indent -- Automatic indentation

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:


;; Auto-indent mode
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil)
(add-hook 'prog-mode-hook 'auto-indent-mode)
;;;; Hack > and < to just indent region when in auto-indent-mode and evil-normal-state
(evil-define-key 'normal auto-indent-mode-map ">" 'indent-region)
(evil-define-key 'normal auto-indent-mode-map "<" 'indent-region)

;; Never use tabs
(setq-default indent-tabs-mode nil)

(provide 'init-auto-indent)
;;; init-auto-indent.el ends here
