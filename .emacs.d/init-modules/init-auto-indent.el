;;; init-auto-indent -- Automatic indentation

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:


;; Auto-indent mode
(require 'init-packages)
(require-package 'auto-indent-mode 'evil)
(add-hook 'prog-mode-hook 'auto-indent-mode)
;;;; Hack >> and << to just indent region when in auto-indent-mode and evil-normal-state
(add-hook 'auto-indent-mode-hook
          (lambda ()
	    (define-key evil-normal-state-map "<" 'indent-region)
             (define-key evil-normal-state-map ">" 'indent-region)))

(provide 'init-auto-indent)
;;; init-auto-indent.el ends here
