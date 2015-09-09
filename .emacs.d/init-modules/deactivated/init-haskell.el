;;; init-haskell -- Haskell development

;;; Commentary:

;;; Code:

;; Haskell mode
(require 'bootstrap)

(require-package 'haskell-mode 'evil 'flycheck  'flycheck-hdevtools 'haskell-interactive-mode 'haskell-process 'auto-complete)
(evil-set-initial-state 'haskell-mode 'normal)

;;; Auto-Indent
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Setup interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; This enables some handy and benign features.
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

;; This gives the basic ways to start a session. In a Haskell buffer:

;; Run C-` to make a REPL open, this will create a session, start GHCi, and open the REPL.
;; Or: run C-c C-l to load the file. This will first try to start a session as the previous command does.
;; Or: run any command which requires a running session. It will always prompt to create one if there isn't one already for the current project.

(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-doc-show-type)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
(define-key haskell-mode-map (kbd "M-,") 'pop-tag-mark)

;; Tag Jumping
(evil-define-key
  'normal haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
(evil-define-key
  'normal haskell-mode-map (kbd "M-,") 'pop-tag-mark)

;; Evil Convenience Commands
(add-hook
 'haskell-mode-hook
 (lambda ()
   (evil-ex-define-cmd "l[oad]" 'haskell-process-load-or-reload)
   (evil-ex-define-cmd "t[ype]" 'haskell-doc-show-type)
   (evil-ex-define-cmd "compile" 'haskell-process-cabal-build)))

;; ;;;; Use hdevtools for on the fly linting / static analysis
(eval-after-load 'flycheck '(require 'flycheck-hdevtools))
(add-hook 'haskell-mode-hook 'flycheck-mode)

;;;; Pretty lambdas for Haskell
;; (defvar haskell-font-lock-symbols)
;; (setq haskell-font-lock-symbols t)

;;;; Auto-complete (requires ghc-mod?)
(add-hook 'haskell-mode-hook 'auto-complete)

(defun haskell-pack-mode-defaults ()
  "Taken from haskell-pack for emacs-live."
  ;https://github.com/ardumont/haskell-pack/blob/master/init.el
  (subword-mode +1)
  ;(turn-on-haskell-doc-mode)
  ;; Ignore compiled Haskell files in filename completions
  (add-to-list 'completion-ignored-extensions ".hi"))

(add-hook 'haskell-mode-hook 'haskell-pack-mode-defaults)

(defun search-hoogle ()
  "Search hoogle for command."
  (interactive)
  (execute-kbd-macro [?\M-x ?h ?o ?o ?g ?l ?e return return]))

;;;; HOOGLE
(setq haskell-hoogle-command "hoogle")
(add-hook
 'haskell-mode-hook
 (lambda ()
   (evil-ex-define-cmd "hoogle" 'search-hoogle)
   (define-key evil-normal-state-map ",?" 'search-hoogle)))

(provide 'init-haskell)
;;; init-haskell.el ends here
