;;; init-elisp -- Emacs lisp configuration

;;; Commentary:

;;; Code:

(require 'init-packages)
(require-package 'flycheck 'evil)

;;;; Use light-table's command-return for evaluating in emacs itself
(define-key emacs-lisp-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
    (if mark-active
	(eval-region (region-beginning) (region-end) t)
        (eval-last-sexp nil))))
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(evil-set-initial-state 'emacs-lisp-mode 'normal)
;;;; Clever hack so lambda shows up as λ
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>"
    (0 (prog1 ()
	 (compose-region (match-beginning 1)
			 (match-end 1)
			 ?λ))))))

(provide 'init-elisp)
;;; init-elisp.el ends here
