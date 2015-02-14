;;; init-javascript -- Emacs as a Javascript IDE

;;; Commentary:

;;; Code:

(require 'bootstrap)

(require-package 'js2-mode 'js-comint)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(evil-set-initial-state 'js2-mode 'normal)
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string
                        ".*1G\.\.\..*5G" "..."
                        (replace-regexp-in-string
                         ".*1G.*3G" "> "
                         output))))))

(provide 'init-javascript)
;;; init-javascript.el ends here
