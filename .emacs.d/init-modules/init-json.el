;;; init-json -- Emacs as a JSON editor

;;; Commentary:

;;; Code:

;; Hy mode
(require 'bootstrap)
(require-package 'json-mode 'flymake-json)

(evil-set-initial-state 'json-mode 'normal)
(add-hook 'json-mode-hook 'flymake-json-load)

(provide 'init-json)
;;; init-json.el ends here
