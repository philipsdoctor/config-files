;;; init-saveplace --- Remember our place
;;; Commentary:
;;; Code:

(require 'saveplace)
(setq-default save-place t)
(defvar saveplace-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where saved-places  reside.")

(setq save-place-file
      (expand-file-name
       (concat saveplace-dir "saved-places")))
(provide 'init-saveplace)
;;; init-saveplace.el ends here
