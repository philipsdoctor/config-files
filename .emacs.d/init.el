;;; init -- Initialization settings

;;; Commentary:

;;; Contains a minimal bootstrap;
;;; other settings should be in init-modules/ directory

;;; Code:

;; Quiet Startup
(setq inhibit-splash-screen t      ; No splash screen
      initial-scratch-message nil  ; No scratch message
      )

(if window-system
  (progn
    (tool-bar-mode -1)         ; No tool-bar
    (scroll-bar-mode -1))      ; No scrollbar (TODO: Change me?)
  (menu-bar-mode -1 )          ; No menubar
  )

;; Custom Initialization Modules
(defvar init-base-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "The basename directory where this init file is located.")
(defvar init-modules-dir (expand-file-name (concat init-base-dir "init-modules/"))
  "The modules directory where user level initialization modules are located; to avoid name-space conflicts all modules should be prefixed with 'init'.")
(add-to-list 'load-path init-modules-dir init-base-dir)

;; Load compiled modules if it exists; else load init modules.
(let ((compiled-modules (expand-file-name (concat init-modules-dir "compiled-modules.elc"))))
  (if (file-exists-p compiled-modules)
      (require 'compiled-modules)
      (mapc 'load (directory-files init-modules-dir nil "^init-.*el$"))))

(provide 'init)
;;; init.el ends here
