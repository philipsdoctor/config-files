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

;; Auto-indent mode
(require-package 'auto-indent-mode 'evil)
(add-hook 'prog-mode-hook 'auto-indent-mode)
;;;; Hack >> and << to just indent region when in auto-indent-mode and evil-normal-state
(add-hook 'auto-indent-mode-hook
          (lambda ()
	    (define-key evil-normal-state-map "<" 'indent-region)
             (define-key evil-normal-state-map ">" 'indent-region)))

;; Flycheck mode
(require 'flycheck)


;; Dash documentation
(when window-system
      (require 'dash-at-point)
      ;;;; EVIL key bindings
      (add-hook 'prog-mode-hook
		(lambda ()
		  (define-key evil-normal-state-map "?" 'dash-at-point))))

;; Rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;;; Set the colors for rainbow-delimiters mode
(set-face-background 'rainbow-delimiters-unmatched-face "red")
(set-face-foreground 'rainbow-delimiters-unmatched-face "black")
(set-face-foreground 'rainbow-delimiters-depth-1-face "#5558dd")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#41a640")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#aa7f00")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#873F88")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#50aea8")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#e0e003")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#f05850")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#0050bb")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#bbbbbb")

;; show-paren-mode
(require 'paren)
(set-face-background 'show-paren-match "white")
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Remember our place
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; TODO: bind enlarge-window-horizontally to-something

;; Smartparen mode
(require 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)
;;;; EVIL key bindings
;;;; TODO: Make smarter
(add-hook 'smartparens-mode-hook
 (lambda ()
   (define-key evil-normal-state-map ",>" 'sp-forward-slurp-sexp)
   (define-key evil-normal-state-map ",." 'sp-forward-barf-sexp)
   (define-key evil-normal-state-map ",," 'sp-backward-slurp-sexp)
   (define-key evil-normal-state-map ",<" 'sp-backward-barf-sexp)))


;; Emacs Lisp mode
;;;; Use light-table's command-return for evaluating in emacs itself
(define-key emacs-lisp-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
    (if mark-active
	(eval-region (region-beginning) (region-end) t)
        (eval-last-sexp nil))))
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(evil-set-initial-state 'emacs-lisp-mode 'normal)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;;;; Clever hack so lambda shows up as λ
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>"
    (0 (prog1 ()
	 (compose-region (match-beginning 1)
			 (match-end 1)
			 ?λ))))))

;; Autocomplete mode
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)

;; Clojure mode
(require 'clojure-mode)
;;;; EVIL mode
(add-hook 'clojure-mode-hook (lambda () (evil-local-mode 1)))
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
;;;; TODO: command-shift-return
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
(eval-after-load 'flycheck '(require 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)
(add-hook 'clojure-mode-hook 'auto-indent-mode)
;;;; Clever hack so fn shows up as λ
(font-lock-add-keywords
 'clojure-mode '(("(\\(fn\\)[\[[:space:]]"
		  (0 (progn (compose-region (match-beginning 1)
					    (match-end 1) "λ")
			    nil)))))


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

