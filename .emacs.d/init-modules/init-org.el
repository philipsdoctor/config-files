;;; init-org --- initialize org mode
;;; Commentary:
;;; Code:
(require 'bootstrap)
(require-package 'org 'evil)
(evil-set-initial-state 'org-mode 'normal)
(define-key org-mode-map "\C-cl" 'org-store-link)
(define-key org-mode-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(provide 'init-org)
;;; init-org.el ends here
