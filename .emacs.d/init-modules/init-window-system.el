;;; init-window-system -- Settings for when emacs runs in a `window-system'

;;; Commentary:

;;; Code:

;; Configure window system

(when window-system
  (setq default-directory "~"             ; Default directory is home directory
	mouse-wheel-scroll-amount '(1)    ; Scroll slowly
	mouse-wheel-progressive-speed nil ; Don't change scrolling speed
	)

  ;; Manipulate font size with usual bindings
  ;;;; To return to default font size, <C-x C-0>
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)

  ;; Set up Emacs' `exec-path' and PATH environment variable.
  ;;;; PATH will match the the user's shell.
  ;;;; This is particularly useful under Mac OS X, where GUI apps
  ;;;; are not started from a shell.
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


(provide 'init-window-system)
;;; init-window-system.el ends here
