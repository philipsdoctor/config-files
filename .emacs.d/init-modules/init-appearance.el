;;; init-appearance -- Set how emacs looks

;;; Commentary:

;;; Code:

(require 'bootstrap)

;; Highlight the current line when programming

(when window-system
  (tool-bar-mode -1)        ; No tool-bar
  (scroll-bar-mode -1)      ; No scrollbar (TODO: Change me?)

  ;; Theming for window mode only
  (require-package 'monokai-theme)
  (load-theme 'monokai t)
  
  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(95 95))
  (add-to-list 'default-frame-alist '(alpha 95 95))

  (custom-set-faces
   '(default ((t (:height 180 :family "Courier New"))))
   '(region ((t (:inherit nil :background "#FFF" :foreground "#00C")))))
  )
  

(when (not window-system)
  (menu-bar-mode -1)                        ; No menubar
  ;(add-hook 'prog-mode-hook 'hl-line-mode)  ; highlight current line

  ;; Set highlight color
  (custom-set-faces
   ;; Make diff readable
   '(cursor ((t (:background "wheat" :foreground "white"))))
   '(diff-added ((t (:inherit nil :background "dark blue"))))
   '(diff-removed ((t (:inherit nil :background "dark red"))))
   '(diff-changed ((t (:inherit nil :background "dark magenta"))))
   '(diff-header ((t (:inherit nil :background "grey10"))))
   '(diff-file-header ((t (:inherit nil :background "grey25"))))
   ;; Use dark blue for selection region
   '(region ((t (:inherit nil :foreground "#FFF" :background "#00F"))))
   ;; Use dim grey for highlights
   '(highlight ((t (:inherit region :background "grey6"))))
   '(hl-line ((t (:inherit highlight))))))

(provide 'init-appearance)
;;; init-appearance.el ends here
