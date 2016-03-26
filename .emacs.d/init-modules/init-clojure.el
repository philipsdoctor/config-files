;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'bootstrap)
(require-package 'clojure-mode 'cider 'flycheck 'evil 'multiple-cursors
                 'clj-refactor 'inf-clojure 'smartparens)


;; Use inferior clojure mode for clojurescript
(add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)

;;;; EVIL mode
(evil-set-initial-state 'clojure-mode 'normal)

;;;; Make EVIL play with cider
(defun cider-jump-immediately ()
  "Make cider jump to a symbol definition immediately."
  (interactive)
  (execute-kbd-macro [?\M-x ?c ?i ?d ?e ?r ?- ?j ?u ?m ?p ?- ?t ?o ?- ?v ?a ?r return return]))

(evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump-immediately)
(evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
(evil-define-key 'motion clojure-mode-map "gd" 'cider-jump-immediately)
(evil-define-key 'motion clojure-mode-map "gb" 'cider-jump-back)
(add-hook
 'clojure-mode-hook
 (lambda ()
   ;;(make-local-variable 'evil-ex-commands)
   ;;(setq evil-ex-commands (copy-list evil-ex-commands))

   ;; Next error
   (evil-ex-define-cmd "next-error" 'cider-jump-to-compilation-error)

   ;; Find usages
   (evil-ex-define-cmd "usages" 'cljr-find-usages)

   ;; Rename a symbol
   ;;(evil-ex-define-cmd "ref[actor]" 'mc/mark-all-like-this-dwim)

   ;; Find a symbol
   (evil-ex-define-cmd "def[inition]" 'cider-jump-to-var)

   ;; Type :ns to change namespaces in cider repl
   (evil-ex-define-cmd "ns" 'cider-repl-set-ns)))

;;;; Use light-table's command-return for evaluating in the REPL
(define-key clojure-mode-map
  command-eval-key
  (lambda () (interactive)
    (cond
     ;; When active region, evaluate region
     ;;;; clojure-mode
     ((and mark-active
           (or (equal major-mode 'clojure-mode)
               (equal major-mode 'clojurec-mode)))
      (cider-eval-region (region-beginning) (region-end)))
     ;;;; clojurescript-mode
     ((and mark-active (equal major-mode 'clojurescript-mode))
      ;;(inf-clojure-eval-region (region-beginning) (region-end))
      (execute-kbd-macro [?\C-c ?\C-r])
      )

     ;; When in normal evil mode and no expression selected, jigger cursor
     ;;;; clojure-mode
     ((and (equal evil-state 'normal)
           (or (equal major-mode 'clojure-mode)
               (equal major-mode 'clojurec-mode)))
      (progn (forward-char) (cider-eval-last-sexp) (backward-char)))
     ;;;; clojurescript-mode
     ((and (equal evil-state 'normal) (equal major-mode 'clojurescript-mode))
      (progn (forward-char)
             ;;(inf-clojure-eval-last-sexp)
             (execute-kbd-macro [?\C-x ?\C-e])
             (backward-char)))

     ;; Default to evaluating last cursor
     ;;;; clojure-mode
     ((or (equal major-mode 'clojure-mode)
          (equal major-mode 'clojurec-mode))
      (cider-eval-last-sexp))

     ;;;; clojurescript-mode
     ((equal major-mode 'clojurescript-mode)
      (execute-kbd-macro [?\C-x ?\C-e])
      ;;(inf-clojure-eval-last-sexp)
      )
     )))

(define-key clojure-mode-map
  command-eval-in-repl-key
  (lambda () (interactive)
    (cond
     ;; When active region, evaluate region
     ;;;; clojure-mode
     ((and mark-active
           (or (equal major-mode 'clojure-mode)
               (equal major-mode 'clojurec-mode)))
      (cider-insert-in-repl (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)) t))
     ;;;; clojurescript-mode
     ((and mark-active (equal major-mode 'clojurescript-mode))
      (inf-clojure-eval-region (region-beginning) (region-end)))

     ;; When in normal evil mode and no expression selected, jigger cursor
     ;;;; clojure-mode
     ((and (equal evil-state 'normal)
           (or (equal major-mode 'clojure-mode)
               (equal major-mode 'clojurec-mode)))
      (progn (forward-char) (cider-insert-last-sexp-in-repl t) (backward-char)))
     ;;;; clojurescript-mode
     ((and (equal evil-state 'normal) (equal major-mode 'clojurescript-mode))
      (progn (forward-char)
             ;;(inf-clojure-eval-last-sexp) ; doesn't work use kbd-macro instead
             (execute-kbd-macro [?\C-x ?\C-e])
             (backward-char)))

     ;; Default to evaluating last cursor
     ;;;; clojure-mode
     ((or (equal major-mode 'clojure-mode)
          (equal major-mode 'clojurec-mode))
      (cider-insert-last-sexp-in-repl t))
     ;;;; clojurescript-mode
     ((equal major-mode 'clojurescript-mode)
      ;;(inf-clojure-eval-last-sexp) ; doesn't work use kbd-macro instead
      (execute-kbd-macro [?\C-x ?\C-e])
      ))))

;; Make smartparens smarter about ` and '
(sp-with-modes '(clojure-mode clojurec-mode clojurescript-mode)
  (sp-local-pair "`" nil :actions nil)
  (sp-local-pair "'" nil :actions nil))

;;;; Use kibit for on the fly static analysis
;;(eval-after-load 'flycheck '(require-package 'kibit-mode))
(add-hook 'clojure-mode-hook 'flycheck-mode)

(defun clojure-indent-file ()
  "Indent a Clojure(Script) file."
  (when (or (equal major-mode 'clojure-mode)
            (equal major-mode 'clojurescript-mode))
    (cljr-sort-ns))
  (when (or (equal major-mode 'clojure-mode)
            (equal major-mode 'clojurec-mode)
            (equal major-mode 'clojurescript-mode))
    (indent-region (point-min) (point-max))))

;; Tidy up file on write
(add-hook 'before-save-hook 'clojure-indent-file)

;; https://github.com/bhauman/lein-figwheel/wiki/Running-figwheel-with-Emacs-Inferior-Clojure-Interaction-Mode
(defun figwheel-repl (&optional build-id)
  "Start a figwheel repl (for clojurescript development).  User may optionally specify a BUILD-ID if they want to use figwheel with a specific build."
  (interactive)
  (when (or (equal major-mode 'clojurescript-mode)
            (equal major-mode 'clojurec-mode))
    (run-clojure (concat "lein figwheel " build-id))))

(when (eq system-type 'darwin)
  (require-package 'dash-at-point)
  (defun set-clj-dash-at-point-docset ()
    "Sets the `dash-at-point-docset` for Clojure."
    (setq dash-at-point-docset "clojure,java8"))
  (add-hook 'clojure-mode-hook 'set-clj-dash-at-point-docset)

  (defun set-cljc-dash-at-point-docset ()
    "Sets the `dash-at-point-docset` for a Clojure .cljc file."
    (setq dash-at-point-docset "clojure,java8,javascript"))
  (add-hook 'clojurec-mode-hook 'set-cljc-dash-at-point-docset)

  (defun set-cljs-dash-at-point-docset ()
    "Sets the `dash-at-point-docset` for ClojureScript."
    (setq dash-at-point-docset "clojure,javascript"))
  (add-hook 'clojurescript-mode-hook 'set-cljs-dash-at-point-docset))

(provide 'init-clojure)
;;; init-clojure.el ends here
