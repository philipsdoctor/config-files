;;; init-packages -- Set up package management system

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("MELPA" . "http://melpa.milkbox.net/packages/" ) t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; TODO: Use a closure here
(defvar *packages-refreshed* nil
  "States whether we have refreshed the packages or not.")
(defun package-refresh-contents-if-necessary ()
  "Run PACKAGE-REFRESH-CONTENTS if it hasn't been run already."
    (when (not *packages-refreshed*)
      (package-refresh-contents)
      (setq *packages-refreshed* t)))

(defmacro require-package (&rest packages)
  "Install given PACKAGES, running PACKAGE-REFRESH-CONTENTS if necessary."
  `(progn
     ,@(mapcar (lambda (p) `(when (not (package-installed-p ,p))
			(package-refresh-contents-if-necessary)
			(package-install ,p))) packages)
     ,@(mapcar (lambda (p) `(require ,p)) packages)))

(require-package 'auto-complete 'clojure-mode 'cider 'goto-last-change 'haskell-mode 'hy-mode 'popup 'rainbow-delimiters 'smex 'undo-tree 'flycheck 'flycheck-hdevtools 'kibit-mode 'smartparens 'auto-indent-mode 'dash-at-point 'puppet-mode)

(provide 'init-packages)
;;; init-packages.el ends here
