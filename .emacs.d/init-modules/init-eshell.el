;;; init-eshell -- Eshell customization

;;; Commentary:

;;; Code:

(defun eshell/vi (file)
  "Hack vi to just open a FILE in eshell."
  (find-file file))
(defun eshell/emacs (file)
  "Hack EMACS to just open a FILE in eshell."
  (find-file file))
(defun eshell/open (file)
  "Hack open to just open a FILE in eshell."
  (find-file file))
(defun eshell/less (file)
  "Hack less to open a FILE in another window."
  (find-file-other-window file))
(defun eshell/more (file)
  "Hack more to open a FILE in another window."
  (find-file-other-window file))
(defun eshell/openo (file)
  "Open a FILE in another window."
  (find-file-other-window file))

(provide 'init-eshell)
;;; init-eshell.el ends here
