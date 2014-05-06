;;; init-evil -- Evil mode

;;; Commentary:

;;; Code:

(require 'package-system-bootstrap)
(require-package 'evil)

;; Enable evil-mode globally
(evil-mode 1)
(setq evil-default-state 'emacs            ; Make default state be emacs
      evil-default-cursor t                ; Use fat cursor by default
      )

;; TODO: Make prog-mode start in normal mode

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

(provide 'init-evil)
;;; init-evil.el ends here
