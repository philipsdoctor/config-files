;;; init-auto-complete --- Autocomplete mode
;;; Commentary:
;;; Code:

(require 'bootstrap)
(require-package 'company)
(add-hook 'prog-mode-hook 'company-mode)
(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
