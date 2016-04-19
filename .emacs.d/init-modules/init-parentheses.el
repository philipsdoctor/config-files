;;; init-parentheses -- Handle parentheses nicely

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:

(require 'bootstrap)

;; Rainbow delimiters
(require-package 'rainbow-delimiters)
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
(require-package 'paren)
(set-face-background 'show-paren-match "blue")
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Smartparen mode
(require-package 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)

;; disable ' in lisp, it's the quote character!
(sp-with-modes sp--lisp-modes
  (sp-local-pair "'" nil :actions nil))

;;;; EVIL key bindings
(require-package 'evil 'evil-leader)
(add-hook 'smartparens-mode-hook 'evil-leader-mode)
(add-hook 'smartparens-mode-hook
          (lambda ()
            ;;;; TODO: Make smarter
            (evil-leader/set-leader ",")
            (evil-leader/set-key
              ">" 'sp-forward-slurp-sexp
              "." 'sp-forward-barf-sexp
              "<" 'sp-backward-slurp-sexp
              "," 'sp-backward-barf-sexp)))

(provide 'init-parentheses)
;;; init-parentheses.el ends here
