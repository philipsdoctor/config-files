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
(defvar compiled-module-basename "compiled-modules.elc"
  "The basename for the byte-compiled initialization modules.")
(add-to-list 'load-path init-modules-dir init-base-dir)

;; Load compiled modules if it exists; else load init modules.
(let ((compiled-modules (expand-file-name (concat init-modules-dir compiled-module-basename))))
  (if (file-exists-p compiled-modules)
      (require 'compiled-modules)
    (mapc 'load (directory-files init-modules-dir nil "^init-.*el$"))))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "c5b920660bf92b790b0173791880085a994bd61ff1fdd11cc766915c642aace5" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(proof-splash-enable nil)
 '(sml/theme (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 180 :family "Courier New"))))
 '(region ((t (:inherit nil :background "#FFF" :foreground "#00C")))))
