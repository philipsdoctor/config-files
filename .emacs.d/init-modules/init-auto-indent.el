;;; init-auto-indent -- Automatic indentation

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:


;; Auto-indent mode
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil)
(add-hook 'prog-mode-hook 'auto-indent-mode)

(defun indent-buffer ()
  "Indent an entire buffer."
  (interactive) (indent-region (point-min) (point-max)))

;; Hack evil so that if you use ">" or "<" it indents the entire buffer
(evil-define-key 'normal auto-indent-mode-map ">" 'indent-buffer)
(evil-define-key 'normal auto-indent-mode-map "<" 'indent-buffer)

;; Never use tabs
(setq-default indent-tabs-mode nil)

(provide 'init-auto-indent)
;;; init-auto-indent.el ends here
