;;; init-clojure -- make emacs a clojure IDE

;;; Commentary:

;;; Code:

;; Clojure mode
(require 'bootstrap)

(require-package
 'flycheck-clojure
 'flycheck
 'cider
 'clojure-mode
 'evil
 'smartparens)

;; Use smart parens
(add-hook 'clojure-mode-hook 'smartparens-mode)


;; Use eldoc mode
(add-hook 'clojure-mode-hook 'eldoc-mode)

;;;; EVIL mode
(evil-set-initial-state 'clojure-mode 'normal)

(add-hook
 'clojure-mode-hook
 (lambda ()
   ;;;; Make EVIL play with cider
   (defun cider-jump-immediately ()
     "Make cider jump to a symbol definition immediately."
     (interactive)
     (execute-kbd-macro [?\M-x ?c ?i ?d ?e ?r ?- ?f ?i ?n ?d ?- ?v ?a ?r return return]))

   (evil-define-key 'normal clojure-mode-map (kbd "M-.") #'cider-jump-immediately)
   (evil-define-key 'normal clojure-mode-map (kbd "M-,") #'cider-pop-back)
   (evil-define-key 'motion clojure-mode-map "gd" #'cider-jump-immediately)
   (evil-define-key 'motion clojure-mode-map "gb" #'cider-pop-back)


   ;; Next error
   (evil-ex-define-cmd "next-error" 'cider-jump-to-compilation-error)

   ;; Rename a symbol
   ;; TODO: leave normal mode for insert mode to do this
   ;;(evil-ex-define-cmd "ref[actor]" 'mc/mark-all-like-this-dwim)

   (setq nrepl-hide-special-buffers nil
         nrepl-log-messages t
         cider-repl-display-help-banner nil
         cider-auto-jump-to-error t
         cider-repl-use-pretty-printing t
         cider-prompt-save-file-on-load nil
         cider-repl-history-file "~/.emacs.d/nrepl-history")

   ;; Find a symbol
   (evil-ex-define-cmd "def[inition]" 'cider-find-var)

   ;; Type :ns to change namespaces in cider repl
   (evil-ex-define-cmd "ns" 'cider-repl-set-ns)

   (custom-set-faces
    '(cider-result-overlay-face ((t (:foreground "#0f0")))))))

;;;; Use light-table's command-return for evaluating in the REPL
(define-key clojure-mode-map
  command-eval-key
  (lambda () (interactive)
    (cond
     ;; When active region, evaluate region
     (mark-active
      (cider-eval-region (region-beginning) (region-end)))

     ;; When in normal evil mode and no expression selected, jigger cursor
     ((equal evil-state 'normal)
      (save-excursion
        (forward-char)
        (cider-eval-last-sexp)))

     (t (cider-eval-last-sexp)))))


(define-key clojure-mode-map
  command-eval-in-repl-key
  (lambda ()
    (interactive)
    ;; TODO: Save-excursion doesn't work here...
    (save-excursion
      (cond
       ;; When active region, evaluate region
       (mark-active
        (cider-insert-in-repl (buffer-substring-no-properties
                               (region-beginning)
                               (region-end)) t))

       ;; When in normal evil mode and no expression selected, jigger cursor
       ((equal evil-state 'normal)
        (forward-char)
        (cider-insert-last-sexp-in-repl t))

       ;; Default to evaluating last sexp
       (t (cider-insert-last-sexp-in-repl t))))))

;; Make smartparens smarter about ` and '
(sp-with-modes '(clojure-mode clojurec-mode clojurescript-mode)
  (sp-local-pair "`" nil :actions nil)
  (sp-local-pair "'" nil :actions nil))

;; TODO: Not working
;;(add-hook 'clojure-mode-hook 'flycheck-mode)
;;(eval-after-load 'flycheck '(flycheck-clojure-setup))

(defun clojure-indent-file ()
  "Indent a Clojure(Script) file."
  (indent-region (point-min) (point-max)))

;; Tidy up file on write
;;(add-hook 'before-save-hook 'clojure-indent-file)

(add-to-list 'cider-jack-in-dependencies '("com.cemerick/piggieback" "0.2.1"))
(defun figwheel-repl (buffer)
  "Start a figwheel repl (for clojurescript development).  Takes BUFFER as an argument."
  (interactive "P")
  ;; TODO: this can't be temporarily bound in the let below for some reason
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl))")
  (let ((cider-jack-in-lein-plugins
         (cons '("lein-figwheel" "0.5.2") cider-jack-in-lein-plugins))
        (cider-jack-in-dependencies
         (cons '("figwheel-sidecar" "0.5.2") cider-jack-in-dependencies)))
    (cider-jack-in-clojurescript)))

(when (eq system-type 'darwin)
  (setenv "LEIN_JVM_OPTS" "-Dapple.awt.UIElement=true")
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
