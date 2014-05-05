;;; init -- Initialization settings

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in modules/ directory

;;; Code:

;; Quiet Startup
(setq inhibit-splash-screen t      ; No splash screen
      initial-scratch-message nil  ; No scratch message
      )

(if window-system
    (progn
	(tool-bar-mode -1)         ; No tool-bar
	(scroll-bar-mode -1))      ; No scrollbar (TODO: Change me?)
    (progn (menu-bar-mode -1 ))    ; No menubar
  )


;; Yes and No
;;;; Nobody likes to have to type out the full yes or no when Emacs asks. Which it does quite often. Make it one character.
(fset 'yes-or-no-p 'y-or-n-p)


;; Custom Initialization Modules
(defvar init-base-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "The basename directory where this init file is located.")
(defvar modules-dir (expand-file-name (concat init-base-dir "modules"))
  "The modules directory where user level initialization modules are located; to avoid name-space conflicts all modules should be prefixed with 'init'.")
(add-to-list 'load-path modules-dir)
(mapc 'load (directory-files modules-dir nil "^[^#].*el$"))


;; Switch to other buffer
(defun switch-to-previous-buffer ()
  "Toggle between this and previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-\\") 'switch-to-previous-buffer)

;; C-tab Goes to other window
(global-set-key [C-tab] 'other-window)

;; Auto-indent mode
(require 'auto-indent-mode)
(add-hook 'prog-mode-hook 'auto-indent-mode)

;; Flycheck mode
(require 'flycheck)

;; EVIL-mode
(require 'evil)
(setq evil-default-cursor t)
;;;; Custom behavior to keep EviL from zealously killing emacs when in window-system
(when window-system
  (defun save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-buffer))

  (define-key evil-normal-state-map "ZZ" 'save-and-kill-buffer)
  (define-key evil-normal-state-map "ZQ" 'evil-delete-buffer)
  (evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer))
;;;; Hack >> and << to just indent region when in auto-indent-mode
(add-hook 'auto-indent-mode-hook
          (lambda ()
	    (define-key evil-normal-state-map "<" 'indent-region)
             (define-key evil-normal-state-map ">" 'indent-region)))

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
;;;; Auto-pair mode is incompatible with smart-parens mode
(add-hook 'smartparens-mode-hook (lambda () (autopair-mode 0)))


;; Emacs Lisp mode
;;;; Use light-table's command-return for evaluating in emacs itself
(define-key emacs-lisp-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
    (if mark-active
	(eval-region (region-beginning) (region-end) t)
        (eval-last-sexp nil))))
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (evil-local-mode 1)))
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

