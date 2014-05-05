;;; init-general -- General initialization settings

;;; Commentary:

;;; Code:

;; Switch to other buffer
(defun switch-to-previous-buffer ()
  "Toggle between this and previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-\\") 'switch-to-previous-buffer)

;; C-tab Goes to other window
(global-set-key [C-tab] 'other-window)

;; Yes and No
;;;; Nobody likes to have to type out the full yes or no when Emacs asks. Which it does quite often. Make it one character.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-general)
;;; init-general.el ends here
