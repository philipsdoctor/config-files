;;; init-dash-at-point -- Dash documentation bindings
;;; Commentary:
;;; Code:

(require 'bootstrap)

(when (eq system-type 'darwin)
  (require-package 'dash-at-point 'evil)

  ;; EVIL key bindings
  (defun make-?-dash-at-point-in-evil-normal-state ()
    "Makes `?` run `dash-at-point` in `evil`'s `normal` state."
    (define-key evil-normal-state-map "?" 'dash-at-point))

  (add-hook
   'prog-mode-hook 'make-?-dash-at-point-in-evil-normal-state))

(provide 'init-dash-at-point)
;;; init-dash-at-point.el ends here
