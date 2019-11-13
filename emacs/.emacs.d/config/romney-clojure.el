;;; romney-clojure.el --- Clojure programming
;;; Commentary:
;;
;;; Code:

(defvar personal/clojure-prettify-alist '()
  "Pretty symbols for Clojure.")

;; (defun personal/find-tag-without-ns (next-p)
;;   "This function will try to find function definitions without their namespace.
;; Accepts a parameter (as NEXT-P), which is unused."
;;   (interactive "P")
;;   (xref-find-definitions
;;    (car (last (split-string (symbol-name (symbol-at-point)) "/")))))

(use-package flycheck-joker
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :defer t
  :defines (clojure--prettify-symbols-alist)
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)
  (define-clojure-indent
    (defroutes 'defun)
    (s/defn 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (with-mock 2)
    (for-all 2)
    (clone-for 2)
    (reg-sub 1)
    (s/fdef 1))
  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   clojure--prettify-symbols-alist))))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :after (clojure-mode))

(use-package cl-lib
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package clojure-snippets
  :ensure t)

(use-package datomic-snippets
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :hook (clojure-mode . cider-mode)
  :bind
  (:map clojure-mode-map
   ("<f5>"     . cider-jack-in)
   ("M-<f5>"   . cider-jack-in-clj&cljs))
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-enlighten-mode)
  (advice-add 'cider-find-var :after #'recenter-top-bottom)
  (setq cider-prefer-local-resources t
        cider-repl-display-help-banner nil
        cider-repl-history-file (expand-file-name "cider-repl.history" user-emacs-directory)
        cider-repl-history-size 1000
        cider-repl-use-pretty-printing t
        cider-pprint-fn 'puget
        cider-prompt-for-symbol nil
        cider-repl-wrap-history t
        cider-jdk-src-paths '("~/src/open/java/clojure-1.9.0"
                              "~/src/open/java/jdk1.8.0")
        cider-default-cljs-repl 'figwheel
        nrepl-hide-special-buffers t
        nrepl-log-messages t)
  (advice-add 'cider-pprint-eval-last-sexp :after #'romney/focus-other-window))

(use-package cider-hydra
  :ensure t
  :hook (cider-mode . cider-hydra-mode))

;; (use-package clj-refactor
;;   :ensure t
;;   :hook (clojure-mode . clj-refactor-mode)
;;   :config
;;   (setq cljr-suppress-middleware-warnings t)
;;   (cljr-add-keybindings-with-prefix "C-c C-a"))

(provide 'romney-clojure)
;;; romney-clojure.el ends here
