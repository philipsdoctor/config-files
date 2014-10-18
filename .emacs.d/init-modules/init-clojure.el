;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'bootstrap)
(require-package 'clojure-mode 'flycheck 'nrepl 'kibit-mode)

;;;; EVIL mode
(require-package 'evil 'cider)
(evil-set-initial-state 'clojure-mode 'normal)
;;;; Make EVIL play with cider
(evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump)
(evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
(evil-define-key 'motion clojure-mode-map "gd" 'cider-jump)
(evil-define-key 'motion clojure-mode-map "gb" 'cider-jump-back)
(add-hook 'clojure-mode-hook
	  (lambda ()
	    ;; Type :ns to change namespaces in cider repl
	    (evil-ex-define-cmd "ns" 'cider-repl-set-ns)
	   ))


;;;; Use light-table's command-return for evaluating in the REPL
(define-key clojure-mode-map
  command-eval-key
  (lambda () (interactive)
    (cond
     (mark-active (cider-eval-region (region-beginning) (region-end)))
     ((equal evil-state 'normal) (progn (forward-char)
					(cider-eval-last-sexp)
					(backward-char)))
     (t (cider-eval-last-sexp)))))

(define-key clojure-mode-map
  command-eval-in-repl-key
  (lambda () (interactive)
    (cond
     (mark-active (cider-insert-in-repl (buffer-substring-no-properties (region-beginning) (region-end)) t))
     ((equal evil-state 'normal) (progn (forward-char)
					(cider-insert-last-sexp-in-repl t)
					(backward-char)))
     (t (cider-insert-last-sexp-in-repl t)))))


;;;; Use kibit for on the fly static analysis
(eval-after-load 'flycheck '(require-package 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)
