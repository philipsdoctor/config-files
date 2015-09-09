;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'bootstrap)
(require-package 'clojure-mode 'cider 'flycheck 'evil 'multiple-cursors 'slamhound 'clj-refactor)

;;;; EVIL mode
(require-package 'evil 'cider)
(evil-set-initial-state 'clojure-mode 'normal)

;;;; Make EVIL play with cider
(defun cider-jump-immediately ()
  "Make cider jump to a symbol definition immediately."
  (interactive)
  (execute-kbd-macro [?\M-x ?c ?i ?d ?e ?r ?- ?j ?u ?m ?p ?- ?t ?o ?- ?v ?a ?r return return]))

(evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump-immediately)
(evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
(evil-define-key 'motion clojure-mode-map "gd" 'cider-jump-immediately)
(evil-define-key 'motion clojure-mode-map "gb" 'cider-jump-back)
(add-hook
 'clojure-mode-hook
 (lambda ()
   ;;(make-local-variable 'evil-ex-commands)
   ;;(setq evil-ex-commands (copy-list evil-ex-commands))

   ;; Next error
   (evil-ex-define-cmd "next-error" 'cider-jump-to-compilation-error)

   ;; Fix namespace
   (evil-ex-define-cmd "slam[hound]" 'slamhound)

   ;; Find usages
   (evil-ex-define-cmd "usages" 'cljr-find-usages)

   ;; Rename a symbol
   ;;(evil-ex-define-cmd "ref[actor]" 'mc/mark-all-like-this-dwim)

   ;; Find a symbol
   (evil-ex-define-cmd "def[inition]" 'cider-jump-to-var)

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

;; Tidy up file on write
(add-hook
 'before-save-hook
 (lambda ()
   (interactive)
   (when (or (equal major-mode 'clojure-mode)
             (equal major-mode 'clojurescript-mode))
     (cljr-sort-ns)
     (indent-region (point-min) (point-max)))))

(provide 'init-clojure)
;;; init-clojure.el ends here
