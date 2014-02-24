;;; init -- Initialization settings 

;;; Commentary:

;;; Code:

;; Quiet Startup
(setq inhibit-splash-screen t          ; No splash screen
      initial-scratch-message nil      ; No scratch message
      )

(if window-system
    (progn 
	(tool-bar-mode -1)                   ; No tool-bar
	(scroll-bar-mode -1))                ; No scrollbar (TODO: Change me?)
    (progn (menu-bar-mode -1 ))
  )

;; Utility Functions
(defun filter (pred lst)
  "Filter a list LST of elements with a given predicate PRED"
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
     This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun add-multiple-hooks (hooks function)
  "Adds a function to multiple hooks"
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;; Yes and No
;;;; Nobody likes to have to type out the full yes or no when Emacs asks. Which it does quite often. Make it one character.
(fset 'yes-or-no-p 'y-or-n-p)

;; Use package system
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/" ) t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;;; my-packages
(defvar my-packages)
(setq my-packages
      '(auto-complete autopair cider color-theme evil goto-last-change haskell-mode hy-mode main-line maxframe nrepl clojure-mode epl popup rainbow-delimiters smex undo-tree flycheck flycheck-hdevtools kibit-mode smartparens auto-indent-mode dash-at-point puppet-mode))

;;;; Install my-packages as necessary
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) my-packages)))
  (when (and (not (equal uninstalled-packages '()))
	     (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))

;; Custom Packages
(add-to-list 'load-path "~/.emacs.d/custom-elisp")

;; Configure window system
(when window-system
  (setq default-directory "~"             ; Default directory is home directory
	mouse-wheel-scroll-amount '(1)    ; Scroll slowly
	mouse-wheel-progressive-speed nil ; Don't change scrolling speed
	)
  ;; Theming for window mode only
  (load-theme 'wombat t)
  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95))
  ;; Use Anonymous Pro font
  (custom-set-faces '(default ((t (:height 180 :family "Anonymous Pro Minus")))))
  ;; Maximize frame by default
  (maximize-frame)
  ;; Manipulate font size with usual bindings
  ;;;; To return to default font size, <C-x C-0>
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (set-exec-path-from-shell-PATH)
  )

;; Switch to other buffer
(defun switch-to-previous-buffer ()
  "Toggle between this and previous buffer"
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

;; evil-mode
(require 'evil)
;;;; Turn on/off evil-mode here
(add-multiple-hooks '(haskell-mode-hook clojure-mode-hook hy-mode-hook emacs-lisp-mode-hook) 'evil-mode)
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

(require 'dash-at-point)
(add-hook 'prog-mode-hook (lambda () (define-key evil-normal-state-map "?" 'dash-at-point)))

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
;; TODO: Make smarter
(add-hook 'smartparens-mode-hook
 (lambda ()
   (define-key evil-normal-state-map ",>" 'sp-forward-slurp-sexp)
   (define-key evil-normal-state-map ",." 'sp-forward-barf-sexp)
   (define-key evil-normal-state-map ",," 'sp-backward-slurp-sexp)
   (define-key evil-normal-state-map ",<" 'sp-backward-barf-sexp)))

;; Emacs Lisp mode
;;;; Use light-table's command-return for evaluating in emacs itself
;;;; TODO: Make smarter
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-last-sexp)  
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)                      ; flycheck-mode 
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (smartparens-mode 1)                              ; better than paredit mode
	    (autopair-mode 0)                                 ; incompatible with smartparens-mode
	    ))
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
;;;; TODO: Does this work in Haskell??
(add-hook 'prog-mode-hook 'auto-complete-mode)
;;;; EviL hack >> and << to just indent region when in auto-indent-mode
(add-hook 'auto-indent-mode-hook
          (lambda ()
             (define-key evil-normal-state-map "<" 'indent-region)
             (define-key evil-normal-state-map ">" 'indent-region)))

;; Clojure mode
(require 'clojure-mode)
;;;; Use light-table's command-return for evaluating in the REPL
;;;; TODO: Make smarter
(define-key clojure-mode-map (kbd "<s-return>") 'cider-eval-last-expression)
;;;; Use kibit for on the fly static analysis
(eval-after-load 'flycheck '(require 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)
;;;; Smartparens mode
(add-hook
 'clojure-mode-hook
 (lambda ()
   (smartparens-mode 1)  ; better than paredit mode
   (autopair-mode 0)     ; incompatible with smartparens-mode
   ))
(add-hook 'clojure-mode-hook 'auto-indent-mode)

;;;; Clever hack so fn shows up as λ
(font-lock-add-keywords
 'clojure-mode '(("(\\(fn\\)[\[[:space:]]"
		  (0 (progn (compose-region (match-beginning 1)
					    (match-end 1) "λ")
			    nil)))))

;;;; Make EviL play with cider correctly
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (evil-ex-define-cmd "ns" 'cider-repl-set-ns)
	    (define-key evil-normal-state-map (kbd "M-.") 'cider-jump)
	    (define-key evil-normal-state-map (kbd "M-,") 'cider-jump-back)
	    ))

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
;;;; TODO: Make smarter
(define-key hy-mode-map (kbd "<s-return>") 'lisp-eval-last-sexp)
(add-hook 'hy-mode-hook
	  (lambda ()
	    (smartparens-mode 1)                ; better than paredit mode
	    (autopair-mode 0)                   ; incompatible with smartparens-mode
	    ))

;; Haskell mode
(require 'haskell-mode)
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
;;;; Use hdevtools to determine type under point
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (load "hdevtools")
	    ;; TODO: overlays in this are fucked :(
	    (define-key evil-normal-state-map "t" 'hdevtools/show-type-info)))

;;;; Dash at point
(require 'dash-at-point)
(define-key evil-normal-state-map "?" 'dash-at-point)

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

