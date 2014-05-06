;;; init-haskell -- Haskell development

;;; Commentary:

;;; Code:

;; Haskell mode
(require 'package-system-bootstrap)
(require-package 'haskell-mode 'evil 'flycheck 'flycheck-hdevtools)

(evil-set-initial-state 'haskell-mode 'normal)

;;;; Use hdevtools for on the fly linting / static analysis
(eval-after-load 'flycheck '(require 'flycheck-hdevtools))
(add-hook 'haskell-mode-hook 'flycheck-mode)
;;;; Auto-indent
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (auto-indent-mode 0)  ; auto-indent-mode is broken for Haskell
	    (turn-on-haskell-indentation)
	    ))
;;;; Auto-complete (requires ghc-mod?)
(add-hook 'haskell-mode-hook 'auto-complete)
;;;; Pretty lambdas for Haskell
(defvar haskell-font-lock-symbols)
(setq haskell-font-lock-symbols t)
;;;; HOOGLE
(setq haskell-hoogle-command "hoogle")
(add-hook
 'haskell-mode-hook
 (lambda ()
   (evil-ex-define-cmd "hoogle" 'hoogle)
   (define-key evil-normal-state-map ",?"
     (lambda ()
       (interactive)
       (execute-kbd-macro [?\M-x ?h ?o ?o ?g ?l ?e return return])))))

(provide 'init-haskell)
;;; init-haskell.el ends here
