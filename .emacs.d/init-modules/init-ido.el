;;; init-ido --- IDO mode
;;; Commentary: A nice way to navigate the filesystem 
;;; Code:

(require 'package-system-bootstrap)
(require-package 'ido)
(ido-mode t)
(defvar ido-enable-flex-matching)
(defvar ido-use-virtual-buffers)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(provide 'init-ido)
;;; init-ido.el ends here
