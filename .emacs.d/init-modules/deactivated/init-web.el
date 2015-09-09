;;; init-web -- Emacs as a WEB editor

;;; Commentary:

;;; Code:

(require 'bootstrap)
(require-package 'web-mode)

(evil-set-initial-state 'web-mode 'normal)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook
 'before-save-hook
 (lambda () (when (equal major-mode 'web-mode)
         (indent-region (point-min) (point-max)))
   nil))
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Blue")


(provide 'init-web)
;;; init-web.el ends here
