;;; init-go --- Go programming environment
;;;
;;; Commentary:
;;;
;;; Code:

(require 'bootstrap)
(require-package 'go-mode 'evil)
(evil-set-initial-state 'go-mode 'normal)
(add-hook
 'go-mode-hook
 (lambda ()
   (add-hook 'before-save-hook 'gofmt-before-save)
   (evil-define-key
     'normal go-mode-map (kbd "M-.") 'godef-jump)
   (evil-define-key
     'normal go-mode-map (kbd "M-,") 'godef-jump-back)
   (evil-define-key 'motion go-mode-map "gd" 'godef-jump)
   (evil-define-key 'motion go-mode-map "gb" 'godef-jump-back)

   (local-set-key (kbd "M-.") 'godef-jump)
   (local-set-key (kbd "M-,") 'godef-jump-back)))

(provide 'init-go)
;;; init-go.el ends here
