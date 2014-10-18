;;; bootstrap -- Bootstrap for the init-module system

;;; Commentary:
;;;
;;; These are settings common to all of the init-modules
;;;
;;; In particular, this sets up the package management system,
;;; and keybindings used in different modes

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


(defvar command-eval-key (kbd "M-RET"))
(defvar command-eval-in-repl-key (kbd "M-S-RET"))

(provide 'bootstrap)
;;; bootstrap.el ends here
