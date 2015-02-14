;;; init-scala -- Emacs as a Scala IDE

;;; Commentary:

;;; Code:

(require 'bootstrap)

(require-package 'scala-mode2 'sbt-mode 'flycheck)
(eval-after-load 'flycheck '(require-package 'scala-mode2))
(add-hook 'scala-mode-hook 'flycheck-mode)
(evil-set-initial-state 'scala-mode 'normal)

(define-key scala-mode-map (kbd "M-.") 'sbt-find-definitions)
;; TODO: Make less dumb
(define-key scala-mode-map command-eval-key 'sbt-send-region)
(define-key scala-mode-map (kbd "C-x '") 'sbt-run-previous-command)

;; TODO: Map C-c C-l

(provide 'init-scala)
;;; init-scala.el ends here
