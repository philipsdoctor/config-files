;;; init -- Emacs Initialization Settings

;;; Commentary:

;;; Code:
;; Quiet Startup
(setq inhibit-splash-screen t             ; No splash screen
      initial-scratch-message nil         ; No scratch message
      )
(when window-system
  (tool-bar-mode -1)                      ; No tool-bar
  (scroll-bar-mode -1)                    ; No scrollbar
)

;; Utility Functions
(defun filter (pred lst)
  "Filter a list LST of elements with a given predicate PRED"
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun add-multiple-hooks (hooks function)
  "Adds a function to multiple hooks"
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;; Yes and No
;;;; Nobody likes to have to type out the full yes or no when Emacs asks. Which it does quite often. Make it one character.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use package system
(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/" ) t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;;; my-packages
(defvar my-packages)
(setq my-packages
      '(auto-complete autopair cider color-theme evil goto-last-change haskell-mode hy-mode main-line maxframe nrepl clojure-mode paredit epl popup rainbow-delimiters smex undo-tree flycheck flycheck-hdevtools kibit-mode auto-indent-mode smartparens dash-at-point))

;;;; Install my-packages as necessary
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x))) my-packages)))
  (when (and (not (equal uninstalled-packages '()))
	     (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))

;; Custom Packages
(add-to-list 'load-path "~/.emacs.d/custom-elisp/")

;; Configure window system
(when window-system
  (setq default-directory "~"             ; Default directory is home directory
	mouse-wheel-scroll-amount '(1)    ; Scroll slowly
	mouse-wheel-progressive-speed nil ; Don't change scrolling speed
	)
  (global-set-key [C-tab] 'other-window)  ; C-tab Goes to other window
  ;; Theming for window mode only
  (load-theme 'wombat t)
  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95))
  ;; Use Anonymous Pro font
  (custom-set-faces '(default ((t (:height 160 :family "Anonymous Pro Minus")))))
  ;; Maximize frame by default
  (maximize-frame))

;; Switch to other buffer
(defun switch-to-previous-buffer ()
  "Toggle between this and previous buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-\\") 'switch-to-previous-buffer)

;; Flycheck mode
(require 'flycheck)

;; evil-mode
(require 'evil)
;;;; Turn on/off evil-mode here
(add-multiple-hooks '(haskell-mode-hook clojure-mode-hook hy-mode-hook emacs-lisp-mode-hook) 'evil-mode)
;;;; BUG: Using evil-mode in Emacs results in the cursor color staying black no matter what theme you use. Work around:
(setq evil-default-cursor t)
;;;; Custom behavior to keep evil from zealously killing emacs when in window-system
(when window-system
  (defun save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-buffer))

  (define-key evil-normal-state-map "ZZ" 'save-and-kill-buffer)
  (define-key evil-normal-state-map "ZQ" 'evil-delete-buffer)
  (evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer))


;; TODO: Doesn't work
(defmacro define-eval-key
  (mode-map key eval-last-sexp-function eval-region-function)
  "Create evil key bindings for setting up an evaluation key"
  `(define-key ,mode-map ,key
     (lambda ()
       (interactive)
       (cond
	((and transient-mark-mode mark-active) (message (funcall ,eval-region-function (point) (mark))))
	((equal evil-state 'normal) (funcall ,eval-last-sexp-function (point)))
	 )
	(t (funcall ,eval-last-sexp-function (point))))))

;; Auto-indent-mode
(require 'auto-indent-mode)
;;;; Hack >> and << to just indent region when in auto-indent-mode
(add-hook 'auto-indent-mode-hook
	  (lambda () 
	    (define-key evil-normal-state-map "<" 'indent-region)
	    (define-key evil-normal-state-map ">" 'indent-region)))

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

;; Smartparen mode
(require 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)
;; TODO: Does this work for you Phil? They may need to be jiggered.
(add-hook 
 'smartparens-mode-hook
 (lambda ()
   (define-key evil-normal-state-map ",>" 'sp-forward-slurp-sexp)
   (define-key evil-normal-state-map ",." 'sp-forward-barf-sexp)
   (define-key evil-normal-state-map ",," 'sp-backward-slurp-sexp)
   (define-key evil-normal-state-map ",<" 'sp-backward-barf-sexp)))

;; Emacs Lisp mode
;;;; Use light-table's command-return for evaluating in emacs itself
;;;; TODO: Make smarter
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-last-sexp)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)    ; On-the-fly linting
(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode) ; Automatic indenting

;; Autocomplete mode
(require 'auto-complete)
(add-hook 'prog-mode-hook 'auto-complete-mode)

;; Clojure mode
(require 'clojure-mode)
;;;; Use light-table's command-return for evaluating in the REPL
;;;; TODO: Make smarter, and jigger slightly
(define-key clojure-mode-map (kbd "<s-return>") 'nrepl-eval-last-expression)
;;;; Use kibit for on the fly linting
(eval-after-load 'flycheck '(require 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)
;;;; Automatic linting
(add-hook 'clojure-mode-hook 'auto-indent-mode)
;;;; TODO: Fix jump to definition for evil meta state

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
;;;; Use light-table's command-return for evaluating in the REPL
;;;; TODO: Make smarter
(define-key hy-mode-map (kbd "<s-return>") 'lisp-eval-last-sexp)

;; Haskell Mode
(require 'haskell-mode)
;;;; Use hdevtools for on the fly linting / static analysis
(eval-after-load 'flycheck '(require 'flycheck-hdevtools))
(add-hook 'haskell-mode-hook 'flycheck-mode)
;;;; Auto-indent
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;;; TODO: Auto-complete (requires ghc-mod?)
;;;; HOOGLE
(setq haskell-hoogle-command "hoogle")
(add-hook 'haskell-mode-hook (lambda () (define-key evil-normal-state-map ",?" (lambda () (interactive) (execute-kbd-macro [?\M-x ?h ?o ?o ?g ?l ?e return return])))))

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
