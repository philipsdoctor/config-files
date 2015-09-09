;;; init-c --- C programming environment
;;;
;;; Commentary:
;;; http://truongtx.me/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/
;;;
;;; Code:

(require 'bootstrap)
(require-package 'cc-mode 'cpputils-cmake 'flycheck 'cl-macs)

;; ggtags
;; https://github.com/leoliu/ggtags
;; - Install exuberant-ctags
;;   > brew install ctags-exuberant
;; - Install pygments
;;   > pip install pygments
;; (add-hook 'c-mode-common-hook 'ggtags-mode)

;; Use CMake when possible for flycheck
(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (and (derived-mode-p 'c-mode 'c++-mode)
              (file-readable-p "CMakeLists.txt"))
     (cppcm-reload-all)
     (flycheck-mode))))

;; Drop into debugger
(global-set-key
 (kbd "C-c C-g")
 '(lambda ()(interactive)
    (gud-gdb (concat "lldb " (cppcm-get-exe-path-current-buffer)))))

;;(setq compilation-scroll-output t)
(defvar compilation-scroll-output 'first-error)
(evil-set-initial-state 'c-mode 'normal)
(evil-set-initial-state 'c++-mode 'normal)

;; TODO: Use Astyle for indenting when possible
(setq c-basic-offset 4
      c-default-style "linux")

;; Compile
(define-key c-mode-map (kbd "C-c C-l") 'compile)
(define-key c++-mode-map (kbd "C-c C-l") 'compile)

(provide 'init-c)
;;; init-c.el ends here
