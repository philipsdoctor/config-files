;;; init-elisp -- Emacs lisp configuration

;;; Commentary:

;;; Code:

(require 'bootstrap)
(require-package 'flycheck 'evil 'multiple-cursors)

(evil-set-initial-state 'emacs-lisp-mode 'normal)

(defun eval-region-or-expression ()
  "Evaluate a region or expression in elisp."
  (interactive)
  (if mark-active
      (eval-region (region-beginning) (region-end) t)
    (eval-last-sexp nil)))

;;;; Meta-return for evaluating in emacs itself
(define-key
  emacs-lisp-mode-map
  command-eval-key
  'eval-region-or-expression)

;;;; C-c C-c for evaluating in emacs itself
(define-key
  emacs-lisp-mode-map
  command-eval-in-repl-key
  'eval-region-or-expression)

(define-key
  emacs-lisp-mode-map
  (kbd "C-c C-l")
  (lambda () (interactive)
    (eval-buffer)
    (message "Elisp evaluated %s"
             (buffer-file-name))))


;; flycheck mode
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;;;; Clever hack so lambda shows up as λ
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>"
    (0 (prog1 ()
         (compose-region
          (match-beginning 1)
          (match-end 1)
          ?λ))))))

(add-hook
 'before-save-hook
 (lambda () (when (equal major-mode 'emacs-lisp-mode)
         (indent-region (point-min) (point-max)))))

(provide 'init-elisp)
;;; init-elisp.el ends here
