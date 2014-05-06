;;; init-auto-complete --- Autocomplete mode
;;; Commentary:
;;; Code:

(require 'bootstrap)
(require-package 'auto-complete 'popup)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
