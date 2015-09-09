;;; init-ethan-wspace --- Whitespace OCD
;;;
;;; Commentary:
;;; https://github.com/glasserc/ethan-wspace
;;;
;;; Code:
(require 'bootstrap)
(require-package 'ethan-wspace)
(setq mode-require-final-newline nil)
(setq-default tab-width 4 indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'ethan-wspace-mode)
(add-hook
 'before-save-hook
 (lambda () (when (not indent-tabs-mode)
         (untabify (point-min) (point-max)))
   nil ))
(provide 'init-ethan-wspace)
;;; init-ethan-wspace.el ends here
