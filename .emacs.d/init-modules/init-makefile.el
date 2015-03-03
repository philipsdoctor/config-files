;;; init-makefile --- Makefiles
;;;
;;; Commentary:
;;;
;;; Code:
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil 'ethan-wspace)
(evil-set-initial-state 'makefile-mode 'normal)
(add-hook
 'makefile-mode-hook
 '(lambda ()
    (setq ethan-wspace-errors
          (remove 'tabs ethan-wspace-errors))
    (setq indent-tabs-mode t)
    (setq auto-indent-mode nil)))

(add-hook
 'makefile-mode-hook
 '(lambda () (setq ethan-wspace-errors
              (remove 'tabs ethan-wspace-errors))))

(provide 'init-makefile)
;;; init-makefile.el ends here
