;;; init-makefile --- Makefiles
;;;
;;; Commentary:
;;;
;;; Code:
(require 'bootstrap)
(require-package 'auto-indent-mode 'evil 'ethan-wspace)
(evil-set-initial-state 'makefile-mode 'normal)

(defun set-Makefile-whitespace-mode ()
  "Set the whitespace settings to accept tabs, in a manner suitable for a Makefile."
  (setq ethan-wspace-errors
        (remove 'tabs ethan-wspace-errors))
  (setq indent-tabs-mode t)
  (setq auto-indent-mode nil))

(add-hook 'makefile-mode-hook 'set-Makefile-whitespace-mode)

(provide 'init-makefile)
;;; init-makefile.el ends here
