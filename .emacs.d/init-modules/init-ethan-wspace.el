;;; init-ethan-wspace --- Whitespace OCD 
;;; Commentary: https://github.com/glasserc/ethan-wspace
;;; Code:
(require-package 'ethan-wspace)
(setq mode-require-final-newline nil)
(add-hook 'prog-mode-hook 'ethan-wspace-mode)
(provide 'init-ethan-wspace)
;;; init-ethan-wspace.el ends here
