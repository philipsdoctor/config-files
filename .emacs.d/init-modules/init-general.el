;;; init-general -- General initialization settings

;;; Commentary:

;;; Code:

;; Switch to other buffer
(defun switch-to-previous-buffer ()
  "Toggle between this and previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-\\") 'switch-to-previous-buffer)

(defun buffer-major-mode ()
  "Display the major mode associated with the current buffer."
  (interactive)
  (message "%s" major-mode))

(defun buffer-minor-modes ()
  "Display the minor modes associated with the current buffer."
  (interactive)
  (message "%s" minor-mode-list))


;; Yes and No
;;;; Nobody likes to have to type out the full yes or no when Emacs asks. Which it does quite often. Make it one character.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-general)
;;; init-general.el ends here
