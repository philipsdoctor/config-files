;;; init-python -- Emacs as a Python IDE

;;; Commentary:

;;; Code:

(require 'bootstrap)
(require-package 'py-autopep8 'flycheck)


(evil-set-initial-state 'python-mode 'normal)
(add-hook 'before-save-hook 'py-autopep8-before-save)

(defun set-pep8-tabs ()
  "Set 'indent-tabs-mode' for compatibility with pep8 standard."
  (setq-local indent-tabs-mode nil
              python-indent 4
              tab-width 4))

;; PEP8 tabs
(add-hook 'python-mode-hook #'set-pep8-tabs)

; (tabify (point-min) (point-max))

(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'init-python)
;;; init-python.el ends here
