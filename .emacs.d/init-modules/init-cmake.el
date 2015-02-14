;;; init-cmake --- CMake
;;;
;;; Commentary:
;;; http://www.cmake.org/Wiki/CMake/Editors/Emacs
;;;
;;; Code:

(require 'bootstrap)
(require-package 'cmake-mode 'cmake-font-lock)
(evil-set-initial-state 'cmake-mode 'normal)
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(provide 'init-cmake)
;;; init-cmake.el ends here
