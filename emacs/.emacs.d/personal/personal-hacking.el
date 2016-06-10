;; Other Languages
(require 'prelude-go)
(require 'prelude-haskell)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-yaml)

;; Projectile caching
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t))

(use-package projectile-direnv
  :ensure t
  :config
  (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables))

;; TAGS management
(use-package ctags
  :ensure t
  :config
  (setq path-to-ctags "/usr/local/bin/ctags"
        projectile-tags-command "/usr/local/bin/ctags -Re %s %s"
        tags-revert-without-query t)
  :bind
  (("M-." . ctags-search)))

;; DevOps, too

(use-package terraform-mode
  :ensure t
  :defer t)

;; (use-package yasnippet
;;   :ensure t
;;   :diminish (yas-minor-mode . " Ŷ")
;;   :config
;;   (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))
;;   (defun do-yas-expand ()
;;     (let ((yas-fallback-behavior 'return-nil))
;;       (yas-expand)))
;;   ;; bind to TAB in keybindings file
;;   (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas-minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common)
;;             (indent-for-tab-command)))))
;;   (add-to-list 'yas-snippet-dirs
;;                (expand-file-name "snippets" prelude-personal-dir))
;;   (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
;;   (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete))
;;
;;
;; (use-package ctags-update
;;  :ensure t
;;  :diminish ctags-auto-update-mode
;;  :commands ctags-update
;;  :config
;;  (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;;  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))


;;; -== Clojure ==-

(require 'prelude-clojure)

(defvar personal/clojure-prettify-alist '()
  "Pretty symbols for Clojure")

(defun personal/find-tag-without-ns (next-p)
  (interactive "P")
  (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "/")))
            next-p))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$" . clojure-mode))

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
    (remove-when 2))

  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   clojure--prettify-symbols-alist)))
  (eval-after-load 'lisp-mode
    '(setq lisp--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   lisp--prettify-symbols-alist)))
  :bind
  ("M-." . personal/find-tag-without-ns))

;;; requires ~/.lein/profiles.clj to have refactor-nrepl
(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-a"))))

(use-package cljr-helm
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-prefer-local-resources t
        cider-repl-display-help-banner nil
        cider-repl-history-file "~/.emacs.d/cider-repl.history"
        cider-repl-history-size 1000
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        nrepl-hide-special-buffers t
        nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

(use-package clojure-snippets
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on))

(use-package datomic-snippets
  :ensure t
  :defer t)

(use-package cljsbuild-mode
  :ensure t
  :defer t)

;;; -== Web ==-
(use-package less-css-mode
  :ensure t
  :defer t)

(require 'prelude-css)
(require 'prelude-scss)
(require 'prelude-web) ;; web-mode 

;; Zen Coding 
(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook 'yas-minor-mode-on))

;;; -== Scheme ==-

(use-package geiser-mode
  :defer t
  :bind ("C-c M-g" . run-geiser)
  :config
  (setq geiser-active-implementations '(guile racket)
        geiser-mode-smart-tab-p t
        geiser-repl-startup-time 10000
        geiser-repl-history-filename "~/.emacs.d/geiser-history"
        geiser-repl-query-on-kill-p nil
        geiser-implementations-alist
        '(((regexp "\\.scm$") guile)
          ((regexp "\\.ss$") guile)
          ((regexp "\\.rkt$") racket)))
  (add-hook 'scheme-mode-hook
            (lambda ()
              (push '("=/=" . ?≢) prettify-symbols-alist)
              (push '("==" . ?≡) prettify-symbols-alist)
              (push '("<=" . ?≤) prettify-symbols-alist)
              (push '(">=" . ?≥) prettify-symbols-alist)))
  (add-hook 'scheme-mode-hook 'yas-minor-mode-on))

;;; -== SQL ==-

(require 'sql)

(defun personal/mysql-connect ()
  (interactive)
  (let ((sql-user (getenv "DB_USER"))
        (sql-password (getenv "DB_PASS"))
        (sql-database (getenv "DB_BASE"))
        (sql-server (getenv "DB_HOST"))
        (sql-port 3306))
    (sql-mysql (current-buffer))))

(global-set-key (kbd "<f10>") 'personal/mysql-connect)
