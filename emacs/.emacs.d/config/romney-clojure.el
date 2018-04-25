;;; romney-clojure.el --- Clojure programming
;;; Commentary:
;;
;;; Code:

(defvar personal/clojure-prettify-alist '()
  "Pretty symbols for Clojure.")

(defun personal/find-tag-without-ns (next-p)
  "This function will try to find function definitions without their namespace.
Accepts a parameter (as NEXT-P), which is unused."
  (interactive "P")
  (xref-find-definitions
   (car (last (split-string (symbol-name (symbol-at-point)) "/")))))

(use-package flycheck-joker
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :defines (clojure--prettify-symbols-alist)
  :bind
  (("M-." . personal/find-tag-without-ns))
  :config
  (require 'clj-refactor)
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
    (alter-when 2)
    (keep-when 2)
    (remove-when 2)
    (reg-sub 1)
    (s/fdef 1))
  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   clojure--prettify-symbols-alist)))
  (eval-after-load 'lisp-mode
    '(setq lisp--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   lisp--prettify-symbols-alist))))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :after (clojure-mode))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-a"))

(use-package helm-clojuredocs
  :ensure t
  :defer t
  :after (helm clojure-mode)
  :bind
  (:map helm-command-map
        ("C-c h d" . helm-clojuredocs-at-point)))

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
  :hook (clojure-mode . cider-mode)
  :bind
  (:map clojure-mode-map
   ("<f5>"   . cider-jack-in)
   ("M-<f5>" . cider-jack-in-clojurescript))
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (advice-add 'cider-find-var :after #'recenter-top-bottom)
  (setq cider-prefer-local-resources t
        cider-repl-display-help-banner nil
        cider-repl-history-file (expand-file-name "cider-repl.history" user-emacs-directory)
        cider-repl-history-size 1000
        cider-repl-use-pretty-printing t
        cider-prompt-for-symbol nil
        cider-repl-wrap-history t
        nrepl-hide-special-buffers t
        nrepl-log-messages t)
  (defadvice cider-pprint-eval-last-sexp
    (after cider-pprint-eval-last-sexp)
    (other-window 1))
  (ad-activate 'cider-pprint-eval-last-sexp))

(use-package sayid
  :ensure t
  :defer t
  :config
  (eval-after-load 'clojure-mode
    '(sayid-setup-package))

  (defadvice sayid-get-workspace
      (after sayid-get-workspace activate)
    (other-window 1))

  (ad-activate 'sayid-get-workspace)

  (defadvice sayid-show-traced
      (after sayid-show-traced activate)
    (other-window 1))

  (ad-activate 'sayid-show-traced))

(provide 'romney-clojure)
;;; romney-clojure.el ends here
