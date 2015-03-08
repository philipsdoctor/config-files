;;; init-company --- Autocomplete mode
;;; Commentary:
;;; Code:

(require 'bootstrap)
(require-package 'company)
(add-hook 'prog-mode-hook 'company-mode)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(provide 'init-company)

;;; init-company.el ends here
