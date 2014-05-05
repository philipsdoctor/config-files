;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'init-packages)
(require-package 'clojure-mode)

;;;; EVIL mode
(require-package 'evil 'cider)
(evil-set-initial-state 'clojure-mode 'normal)
;;;; Make EVIL play with cider correctly
(add-hook 'clojure-mode-hook
	  (lambda ()
	    ;; Type :ns to change namespaces in cider repl
	    (evil-ex-define-cmd "ns" 'cider-repl-set-ns)
	    ;; Use M-. to jump to definition
	    (define-key evil-normal-state-map (kbd "M-.") 'cider-jump)
	    ;; Use M-, to jump back after jumping to definition
	    (define-key evil-normal-state-map (kbd "M-,") 'cider-jump-back)))

;;;; Use light-table's command-return for evaluating in the REPL
(define-key clojure-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
    (cond
     (mark-active (cider-eval-region (region-beginning) (region-end)))
     ((equal evil-state 'normal) (progn (forward-char)
					(cider-eval-last-sexp)
					(backward-char)))
     (t (cider-eval-last-sexp)))))

(define-key clojure-mode-map
  (kbd "<s-S-return>")
  (lambda () (interactive)
    (cond
     (mark-active (cider-insert-in-repl (buffer-substring-no-properties (region-beginning) (region-end)) t))
     ((equal evil-state 'normal) (progn (forward-char)
					(cider-insert-last-sexp-in-repl t)
					(backward-char)))
     (t (cider-insert-last-sexp-in-repl t)))))


;;;; Use kibit for on the fly static analysis
(require-package 'flycheck 'kibit-mode)
(eval-after-load 'flycheck '(require 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)

;;;; Use smart parens (as opposed to paraedit)
(require-package 'smartparens)
(add-hook 'clojure-mode-hook 'smartparens-mode)

;;;; Clever hack so fn shows up as λ
(font-lock-add-keywords
 'clojure-mode '(("(\\(fn\\)[\[[:space:]]"
		  (0 (progn (compose-region (match-beginning 1)
					    (match-end 1) "λ")
			    nil)))))
(provide 'init-clojure)
;;; init-clojure.el ends here
