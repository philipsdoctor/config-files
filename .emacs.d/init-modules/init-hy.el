;;; init-hy -- Emacs as a Hy IDE

;;; Commentary:

;;; Code:

;; Hy mode
(require 'package-system-bootstrap)
(require-package 'hy-mode 'evil)

(evil-set-initial-state 'hy-mode 'normal)

; Bug where cl-flet can't be found
(unless (fboundp 'cl-flet)
  (defalias 'cl-flet 'flet))

;;;; Use light-table's command-return for evaluating in the REPL
(define-key hy-mode-map
  (kbd "<s-return>")
  (lambda () (interactive)
    (cond
     (mark-active (lisp-eval-region (region-beginning) (region-end)))
     ((equal evil-state 'normal) (progn (forward-char)
                                        (lisp-eval-last-sexp)
                                        (backward-char)))
     (t (lisp-eval-last-sexp)))))


(provide 'init-hy)
;;; init-hy.el ends here
