(require 'clojure-snippets)

;; Clojure Programming
(setq auto-mode-alist (cons '("\\.edn$"  . clojure-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.clj(x|s)?$" . clojure-mode) auto-mode-alist))

(setq cider-repl-use-pretty-printing t)

(setq nrepl-hide-special-buffers t)

;; open local files before remote ones
(setq cider-prefer-local-resources t)

;; see what the f is going on
(setq nrepl-log-messages t)

;; Emacs complains about these being free variables
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.emacs.d/cider-repl.history")

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

;; Make fn look nice
(add-hook 'clojure-mode-hook
          (lambda ()
            (push '("fn" . ?𝑓) prettify-symbols-alist)
            (push '("!=" . ?≢) prettify-symbols-alist)
            (push '("==" . ?≡) prettify-symbols-alist)
            (push '("->" . ?⇝) prettify-symbols-alist)
            (push '("->>" . ?↠) prettify-symbols-alist)
            (push '(">!" . ?⥅) prettify-symbols-alist)
            (push '(">!!" . ?↬) prettify-symbols-alist)
            (push '("<!" . ?⥆) prettify-symbols-alist)
            (push '("<!!" . ?↫) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("not=" . ?≠) prettify-symbols-alist)
            ))

;; Refactor (requires ~/.lein/profiles.clj to have refactor-nrepl)
(require 'clj-refactor)
(add-hook 'clojure-mode-hook 
          (lambda ()
            (clj-refactor-mode 1)
            ;; NO
            (cljr-add-keybindings-with-prefix "C-c C-a")))

;; Compojure indentation
(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (s/defn 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
