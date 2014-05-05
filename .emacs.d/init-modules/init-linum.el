;;; init-linum --- initialize linum mode
;;; Commentary:
;;; Code:
(defvar linum-format "%d " "Format string for 'linum-mode''.")
(add-hook 'prog-mode-hook 'linum-mode)
(provide 'init-linum)
;;; init-linum.el ends here
