;;; init-yaml -- Emacs as a YAML editor

;;; Commentary:

;;; Code:

;; Hy mode
(require 'bootstrap)
(require-package 'yaml-mode 'flymake-yaml)

(evil-set-initial-state 'yaml-mode 'normal)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)

(provide 'init-yaml)
;;; init-yaml.el ends here
