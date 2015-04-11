;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'bootstrap)
(require-package 'clojure-mode 'flycheck 'evil)

;;;; EVIL mode
(require-package 'evil 'cider)
(evil-set-initial-state 'clojure-mode 'normal)


(defun cider-jump-immediately ()
  "Make cider jump to a symbol definition immediately."
  (interactive)
  (execute-kbd-macro [?\M-x ?c ?i ?d ?e ?r ?- ?j ?u ?m ?p return return]))

;;;; Make EVIL play with cider
(evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump-immediately)
(evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
(evil-define-key 'motion clojure-mode-map "gd" 'cider-jump-immediately)
(evil-define-key 'motion clojure-mode-map "gb" 'cider-jump-back)
(add-hook
 'clojure-mode-hook
 (lambda ()
   ;; Find a symbol
   (evil-ex-define-cmd "find" 'cide-jump)
   ;; Type :ns to change namespaces in cider repl
   (evil-ex-define-cmd "ns" 'cider-repl-set-ns)))


;;;; Use light-table's command-return for evaluating in the REPL
(define-key clojure-mode-map
  command-eval-key
  (lambda () (interactive)
    (cond
     (mark-active
      (cider-eval-region (region-beginning)
                         (region-end)))
     ((equal evil-state 'normal)
      (progn (forward-char)
             (cider-eval-last-sexp)
             (backward-char)))
     (t (cider-eval-last-sexp)))))

(define-key clojure-mode-map
  command-eval-in-repl-key
  (lambda () (interactive)
    (cond
     (mark-active
      (cider-insert-in-repl
       (buffer-substring-no-properties
        (region-beginning)
        (region-end)) t))
     ((equal evil-state 'normal)
      (progn (forward-char)
             (cider-insert-last-sexp-in-repl t)
             (backward-char)))
     (t (cider-insert-last-sexp-in-repl t)))))


;;;; Use kibit for on the fly static analysis
;;(eval-after-load 'flycheck '(require-package 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)

(add-hook
 'before-save-hook
 (lambda () (when (equal major-mode 'clojure-mode)
         (indent-region (point-min) (point-max)))
   nil))

(provide 'init-clojure)
;;; init-clojure.el ends here
