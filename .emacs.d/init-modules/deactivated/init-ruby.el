;;; init-ruby -- Emacs as a Ruby IDE

;;; Commentary:

;;; Code:

(require 'bootstrap)

;(require-package 'enh-ruby-mode)
;(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
;(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(evil-set-initial-state 'ruby-mode 'normal)
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq require-final-newline nil)
            (setq mode-require-final-newline nil)))

(provide 'init-ruby)
;;; init-ruby.el ends here
