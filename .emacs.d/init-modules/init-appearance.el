;;; init-appearance -- Set how emacs looks

;;; Commentary:

;;; Code:

(require 'init-packages)
(require-package 'main-line)

(when window-system
  (tool-bar-mode -1)        ; No tool-bar
  (scroll-bar-mode -1)      ; No scrollbar (TODO: Change me?)

  ;; Theming for window mode only
  (require 'init-packages)
  (require-package 'color-theme)
  (load-theme 'wombat t)    ; Use Wombat Theme

  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95))

  ;; Use Anonymous Pro font
  (custom-set-faces '(default ((t (:height 180 :family "Anonymous Pro")))))
  )
  

(when (not window-system)
  (progn (menu-bar-mode -1 ))    ; No menubar
  )



(provide 'init-appearance)
;;; init-appearance.el ends here
