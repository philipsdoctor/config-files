;;; init-evil -- Evil mode

;;; Commentary:

;;; Code:

(require 'bootstrap)
(require-package 'evil
         ;'surround
         )

;; Enable evil-mode globally
(evil-mode 1)
(setq
 evil-default-state 'emacs            ; Make default state be emacs
 evil-default-cursor t                ; Use default emacs cursor
 )

;; All prog-modes start in normal state
(add-hook 'prog-mode-hook (lambda () (interactive) (evil-set-initial-state major-mode 'normal)))
;(add-hook 'prog-mode-hook 'turn-on-surround-mode) ; "ds(" deletes parens in normal state

;;;; Custom behavior to keep evil from zealously killing emacs when in window-system
;;;; TODO: Incorporate http://zuttobenkyou.wordpress.com/category/emacs/
(when window-system
  (defun save-and-kill-buffer ()
    (interactive)
    (save-buffer)
    (kill-buffer))

  (define-key evil-normal-state-map "ZZ" 'save-and-kill-buffer)
  (define-key evil-normal-state-map "ZQ" 'evil-delete-buffer)
  (evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)
  (evil-ex-define-cmd "wq" 'save-and-kill-buffer))

; Override the default x-select-text function because it doesn't
; respect x-select-enable-clipboard on OS X.
(defun x-select-text (text)
  "Dummy function that does nothing with TEXT.")
(defvar x-select-enable-clipboard nil)
(defvar x-select-enable-primary nil)
(defvar mouse-drag-copy-region nil)

(setq interprogram-cut-function 'ns-set-pasteboard)
(setq interprogram-paste-function 'ns-get-pasteboard)

(provide 'init-evil)
;;; init-evil.el ends here
