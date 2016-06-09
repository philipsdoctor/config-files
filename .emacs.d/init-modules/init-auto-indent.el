;;; init-auto-indent -- Automatic indentation

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:


;; Auto-indent mode
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil)
(add-hook 'prog-mode-hook 'auto-indent-mode)

(defun indent ()
  "Indent a region or line."
  (interactive)
  (cond
   (mark-active (indent-region (region-beginning) (region-end)))
   (t (indent-region (line-beginning-position) (line-end-position)))))

;; Hack evil so that if you use ">" or "<" it indents the entire buffer
(evil-define-key 'normal auto-indent-mode-map ">" 'indent)
(evil-define-key 'normal auto-indent-mode-map "<" 'indent)

;; Never use tabs
(setq-default indent-tabs-mode nil)

(provide 'init-auto-indent)
;;; init-auto-indent.el ends here
