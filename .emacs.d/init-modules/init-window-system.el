;;; init-window-system -- Settings for when emacs runs in a `window-system'

;;; Commentary:

;;; Code:

;; Configure window system

(if window-system
    (progn
      (setq default-directory "~")             ; Default directory is home directory

      ;; Manipulate font size with usual bindings
      ;; To return to default font size, <C-x C-0>
      (global-set-key (kbd "s-=") 'text-scale-increase)
      (global-set-key (kbd "s-+") 'text-scale-increase)
      (global-set-key (kbd "s--") 'text-scale-decrease)

      ;; Set up Emacs' `exec-path' and PATH environment variable.
      ;; PATH will match the the user's shell.
      ;; This is particularly useful under Mac OS X, where GUI apps
      ;; are not started from a shell.
      (setenv "TERM" "xterm-color")
      (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))

  ;; else, if we aren't in a windowing system

  (progn
    ;; Set up the console mouse
    (require 'mouse)
    (xterm-mouse-mode t)          ; Toggle with xterm-mouse-mode
    (defun track-mouse (e))
    (defvar mouse-wheel-mode t)
    (defvar mouse-sel-mode t)
    (defun up-slightly () (interactive) (scroll-up 1))
    (defun down-slightly () (interactive) (scroll-down 1))
    (global-set-key (kbd "<mouse-4>") 'down-slightly)
    (global-set-key (kbd "<mouse-5>") 'up-slightly)

    ;; CUA-Mode
    (cua-mode t)
    ;; Don't tabify after rectangle commands
    (defvar cua-auto-tabify-rectangles nil)
    ;; No region when it is not highlighted
    (transient-mark-mode 1)
    ;; Standard Windows behaviour
    (defvar cua-keep-region-after-copy t)
    ;; Rectangle support, global mark mode,
    ;; and other features, but standard Emacs keys
    (cua-selection-mode t)
    ;; shift + click select region
    (define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
    (define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
    (put 'mouse-set-point 'CUA 'move)))

(defvar mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; One line at a time
(defvar mouse-wheel-progressive-speed nil)            ;; Don't accelerate scrolling
(defvar mouse-wheel-follow-mouse 't)                  ;; Scroll window under mouse
(defvar scroll-step 1)

(global-set-key (kbd "ESC <up>") 'shrink-window)
(global-set-key (kbd "ESC <down>") 'enlarge-window)
(global-set-key (kbd "ESC <left>") 'shrink-window-horizontally)
(global-set-key (kbd "ESC <right>") 'enlarge-window-horizontally)

(provide 'init-window-system)
;;; init-window-system.el ends here
