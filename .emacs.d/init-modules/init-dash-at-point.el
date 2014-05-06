;;; init-dash-at-point -- Dash documentation bindings

;;; Commentary:

;;; Code:


(require 'package-system-bootstrap)
(require-package 'dash-at-point 'evil)
;;;; EVIL key bindings
(add-hook 'prog-mode-hook
	  (lambda ()
	    (define-key evil-normal-state-map "?" 'dash-at-point)))

(provide 'init-dash-at-point)
;;; init-dash-at-point.el ends here
