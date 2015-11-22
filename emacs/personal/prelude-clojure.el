(use-package clojure-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
    (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$" . clojure-mode))
    (bind-key "C-c C-m a l" 'align-cljlet clojure-mode-map)
    (add-hook 'clojure-mode-hook
              (lambda ()
                (push '("fn" . ?𝝺) prettify-symbols-alist)
                (push '("!=" . ?≢) prettify-symbols-alist)
                (push '("==" . ?≡) prettify-symbols-alist)
                (push '("<=" . ?≤) prettify-symbols-alist)
                (push '(">=" . ?≥) prettify-symbols-alist)
                (push '("not=" . ?≠) prettify-symbols-alist) ))
    (define-clojure-indent
      (defroutes 'defun)
      (s/defn 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2))))

;;; requires ~/.lein/profiles.clj to have refactor-nrepl
(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-a"))))

(use-package cider-mode
  :config
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-prefer-local-resources t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-repl-history-file "~/.emacs.d/cider-repl.history")
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode))

(use-package clojure-snippets
  :ensure t)

(provide 'personal/prelude-clojure)
