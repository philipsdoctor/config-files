;;; init-html -- Emacs as an HTML IDE

;;; Commentary:

;;; Code:

;; Hy mode
(require 'bootstrap)

(evil-set-initial-state 'html-mode 'normal)
(add-hook 'html-mode-hook 'flycheck-mode)


(provide 'init-hy)
;;; init-html.el ends here
