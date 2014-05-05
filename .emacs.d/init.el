;;; init -- Initialization settings

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:

;; Quiet Startup
(setq inhibit-splash-screen t      ; No splash screen
      initial-scratch-message nil  ; No scratch message
      )

;; Custom Initialization Modules
(defvar init-base-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "The basename directory where this init file is located.")
(defvar init-modules-dir (expand-file-name (concat init-base-dir "init-modules"))
  "The modules directory where user level initialization modules are located; to avoid name-space conflicts all modules should be prefixed with 'init'.")
(add-to-list 'load-path init-modules-dir)
(mapc 'load (directory-files init-modules-dir nil "^[^#].*el$"))

;; Remember our place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; TODO: bind enlarge-window-horizontally to-something

;; Autocomplete mode
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)


;; Display line number
(add-hook 'prog-mode-hook 'linum-mode)

;; IDO mode
;; A nice way to navigate the filesystem
(ido-mode t)
(defvar ido-enable-flex-matching)
(defvar ido-use-virtual-buffers)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

;; Hy mode
(require 'hy-mode)
; Bug where cl-flet can't be found
(unless (fboundp 'cl-flet)
  (defalias 'cl-flet 'flet))
;;;; Use light-table's command-return for evaluating in the REPL
(define-key hy-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
(cond
     (mark-active (lisp-eval-region (region-beginning) (region-end)))
     ((equal evil-state 'normal) (progn (forward-char)
					(lisp-eval-last-sexp)
					(backward-char)))
     (t (lisp-eval-last-sexp)))
    ))
;(define-key hy-mode-map (kbd "<s-return>") 'lisp-eval-last-sexp)
(add-hook 'hy-mode-hook 'smarparens-mode)
(add-hook 'hy-mode-hook (lambda () (evil-local-mode 1)))

;; Haskell mode
(require 'haskell-mode)
;;;; Use hdevtools for on the fly linting / static analysis
(eval-after-load 'flycheck '(require 'flycheck-hdevtools))
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook (lambda () (evil-local-mode 1)))
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

;; EShell stuff
(add-hook
 'eshell-mode
 (lambda ()
   (defalias 'vi 'find-file)
   (defalias 'less 'find-file-other-window)
   (defalias 'more 'find-file-other-window)
   (defalias 'openo 'find-file-other-window)
   ))

(provide 'init)
;;; init.el ends here
