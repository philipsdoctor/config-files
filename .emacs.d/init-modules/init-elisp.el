;;; init-elisp -- Emacs lisp configuration
;;;
;;; Commentary:
;;;
;;; Code:

(require 'bootstrap)

(require-package 'flycheck 'evil 'smartparens)

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

;; Make elisp smarter about single quote
(sp-with-modes '(elisp-mode) (sp-local-pair "'" nil :actions nil))
(sp-with-modes '(elisp-mode) (sp-local-pair "`" nil :actions nil))

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

(defun elisp-indent-file ()
  "Indent an elisp file."
  (when (equal major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max))))

(add-hook 'before-save-hook 'elisp-indent-file)

(provide 'init-elisp)
;;; init-elisp.el ends here
