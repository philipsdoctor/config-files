;;; init-c --- C programming environment
;;;
;;; Commentary:
;;; http://truongtx.me/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/
;;;
;;; Code:

(require 'bootstrap)
(require-package 'cc-mode 'cpputils-cmake 'flycheck 'cl-macs)
(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (and (derived-mode-p 'c-mode 'c++-mode)
              (file-readable-p "CMakeLists.txt"))
     (cppcm-reload-all)
     (flycheck-mode))))

(global-set-key
 (kbd "C-c C-g")
 '(lambda ()(interactive)
    (gud-gdb (concat "lldb " (cppcm-get-exe-path-current-buffer)))))

;(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)
(evil-set-initial-state 'c-mode 'normal)
(evil-set-initial-state 'c++-mode 'normal)
(setq c-basic-offset 4
      c-default-style "linux")
(define-key c-mode-map (kbd "C-c C-l") 'compile)
(define-key c++-mode-map (kbd "C-c C-l") 'compile)

;; TODO: Use Astyle for indenting when possible

;;(defun find-file-up (file-name)
;; "Recursively search parent directories for a specified FILE-NAME."
;;  t)

(provide 'init-c)
;;; init-c.el ends here
