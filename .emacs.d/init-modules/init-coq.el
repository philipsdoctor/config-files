;;; init-coq --- Formal Verification
;;;
;;; Commentary:
;;;
;;; Code:
(require 'bootstrap)

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.  White space here is any of: space, tab, EMACS newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun parent-directory (f)
  "Get the parent directory for a file F."
  (file-name-directory (directory-file-name f)))

(when (file-exists-p (trim-string (shell-command-to-string "which coqtop")))
  (defvar init-base-dir
    (parent-directory
     (parent-directory
      (or load-file-name (buffer-file-name))))
    "The base directory where.")

  (defvar proof-general-home
    (expand-file-name (concat init-base-dir "site-lisp/" "ProofGeneral/"))
    "Directory where proof general is installed.")

  (when (not (file-directory-p proof-general-home))
    (shell-command (concat "mkdir -p " (parent-directory proof-general-home)))
    (message "Downloading Proof General...")
    (shell-command (concat "git clone https://github.com/ProofGeneral/PG " proof-general-home))
    (message "Compiling Proof General...")
    (shell-command (concat "make -C " proof-general-home)))

  (defvar cpdt-home
    (expand-file-name (concat init-base-dir "coq/" "cpdt/"))
    "Directory where Coq libraries are installed.")

  (when (not (file-directory-p cpdt-home))
    (shell-command (concat "mkdir -p " (parent-directory cpdt-home)))
    (message "Downloading CPDT...")
    (shell-command (concat "wget -qO- http://adam.chlipala.net/cpdt/cpdt.tgz | tar zx -C " (parent-directory cpdt-home)))
    (message "Compiling CPDT...")
    (shell-command (concat "make -C " cpdt-home)))

  ;; (concat cpdt-home "src/")

  (defvar proof-general-elisp
    (expand-file-name (concat proof-general-home "generic/"))
    "Directory containing proof general elisp libraries.")
  (add-to-list 'load-path (concat proof-general-home "generic/"))

  (add-hook
   'after-init-hook
   (lambda ()
     (defvar coq-site-lisp
       (concat
        (parent-directory
         (parent-directory
          (trim-string (shell-command-to-string "coqtop -config | grep DOCDIR | cut -d= -f2"))))
        "emacs/" "site-lisp/" "coq/")
       "Directory containing elisp libraries for Coq.")
     (add-to-list 'load-path coq-site-lisp)
     (require-package 'evil 'gallina 'proof-site 'company-coq)
     (autoload 'coq-mode "gallina" "Major mode for editing Coq vernacular." t)
     (evil-set-initial-state 'coq-mode 'normal)
     (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))

     ;; Silence company-coq startup message
     (defun company-coq--hello ())

     (add-hook 'coq-mode-hook
               (lambda ()
                 (custom-set-variables `(coq-prog-args `("-R" ,,(concat cpdt-home "src") "Cpdt")))
                 (defvar coq-mode-map nil)
                 (defvar company-coq-features/prettify-symbols-in-terminals t)
                 (when (and (fboundp 'proof-goto-point) (not window-system))
                   (define-key coq-mode-map (kbd "C-c C-M-j") #'proof-goto-point)
                   (define-key coq-mode-map (kbd "C-M-j") #'proof-goto-point)
                   (evil-define-key 'normal coq-mode-map (kbd "C-c C-M-j") #'proof-goto-point)
                   (evil-define-key 'normal coq-mode-map (kbd "C-M-j") #'proof-goto-point))
                 (custom-set-variables '(proof-splash-enable nil))
                 (when (fboundp 'company-coq-mode) (company-coq-mode t)))))))

(provide 'init-coq)
;;; init-coq.el ends here
