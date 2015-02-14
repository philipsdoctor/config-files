;;; init-ethan-wspace --- Whitespace OCD
;;;
;;; Commentary:
;;; https://github.com/glasserc/ethan-wspace
;;;
;;; Code:
(require 'bootstrap)
(require-package 'ethan-wspace)
(setq mode-require-final-newline nil)
(setq-default tab-width 4 indent-tabs-mode t)
(add-hook 'prog-mode-hook 'ethan-wspace-mode)
(add-hook
 'makefile-mode-hook
 '(lambda () (setq ethan-wspace-errors
              (remove 'tabs ethan-wspace-errors))))
(provide 'init-ethan-wspace)
;;; init-ethan-wspace.el ends here
