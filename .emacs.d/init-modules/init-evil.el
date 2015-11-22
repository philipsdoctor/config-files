;;; init-evil -- Evil mode

;;; Commentary:

;;; Code:

(require 'bootstrap)
(require-package 'evil)

;; Enable evil-mode globally
(evil-mode 1)
(setq
 evil-default-state 'emacs            ; Make default state be emacs
 evil-default-cursor t                ; Use default emacs cursor
 )

;; TODO: More modes?
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)

;; All prog-modes start in normal state
(defun set-evil-initial-state-in-normal-mode ()
  "Puts evil in `normal` mode in its initial state."
  (interactive) (evil-set-initial-state major-mode 'normal))
(add-hook 'prog-mode-hook 'set-evil-initial-state-in-normal-mode)

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

(provide 'init-evil)
;;; init-evil.el ends here
