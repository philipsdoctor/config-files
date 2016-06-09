;;; bootstrap -- Bootstrap for the init-module system

;;; Commentary:
;;;
;;; These are settings common to all of the init-modules
;;;
;;; In particular, this sets up the package management system,
;;; and keybindings used in different modes

;;; Code:

(require 'package)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/" ) t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)
(package-initialize)

;; TODO: Use a closure here... lol does elisp even support this lisp feature from 1975?
(defvar *quelpa-refreshed* nil
  "States whether we have refreshed quelpa after boot.")
(defun quelpa-refresh-if-necessary ()
  "Refresh quelpa if it hasn't been already."
  (when (not *quelpa-refreshed*)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
      (eval-buffer))
    (setq *quelpa-refreshed* t)))

(unless (require 'quelpa nil t)
  (quelpa-refresh-if-necessary))

(defvar *packages-refreshed* nil
  "States whether we have refreshed the packages or not after boot.")
(defun package-refresh-contents-if-necessary ()
  "Run PACKAGE-REFRESH-CONTENTS if it hasn't been run already."
  (when (not *packages-refreshed*)
    (package-refresh-contents)
    (setq *packages-refreshed* t)))

(defmacro require-package (&rest packages)
  "Install given PACKAGES, running PACKAGE-REFRESH-CONTENTS if necessary."
  `(progn
     ,@(mapcar
        (lambda (p)
          (cond
           ((symbolp (cadr p)) `(when (not (or (require ,p nil 'noerror)
                                               (package-installed-p ,p)))
                                  (package-refresh-contents-if-necessary)
                                  (condition-case err
                                      (package-install ,p)
                                    (error (quelpa ,p)))))
           ((listp (cadr p)) `(when (not (or (require ',(car (cadr p)) nil 'noerror)
                                             (package-installed-p ',(car (cadr p)))))
                                (package-refresh-contents-if-necessary)
                                (quelpa ,p)))))
        packages)
     ,@(mapcar (lambda (p)
                 (cond ((symbolp (cadr p)) `(require ,p))
                       ((listp (cadr p)) `(require ',(car (cadr p))))))
               packages)))


(defvar command-eval-key (kbd "M-RET"))
;; Note: you may wish to set C-RET or M-SHIFT-RET to emit send the hex code "0x18 0x0A" in iTerm
(defvar command-eval-in-repl-key (kbd "C-M-j"))

(provide 'bootstrap)
;;; bootstrap.el ends here
