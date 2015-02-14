;;; init-makefile --- Makefiles
;;;
;;; Commentary:
;;;
;;; Code:
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil)
(evil-set-initial-state 'makefile-mode 'normal)
(add-hook
 'makefile-mode-hook
 '(lambda ()
    (setq ethan-wspace-errors
          (remove 'tabs ethan-wspace-errors))
    (setq indent-tabs-mode t)
    (setq auto-indent-mode nil)))

(provide 'init-makefile)
;;; init-makefile.el ends here
