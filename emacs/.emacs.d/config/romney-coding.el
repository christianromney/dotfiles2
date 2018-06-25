;;; romney-coding.el --- General coding settings and utility packages
;;; Commentary:
;;
;;; Code:
;; everything should indent w/ 2 spaces
(setq-default c-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default elixir-basic-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default sgml-basic-offset 2)

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-x g" . magit-status))
  :init
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package magithub
  :ensure t
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :bind ("C-c g" . git-timemachine-toggle))

(use-package projectile
  :ensure t
  :defer t
  :defines (helm-completion-system)
  :diminish projectile-mode
  :config
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" personal-savefile-dir))
  (projectile-mode t))

(use-package helm-projectile
  :ensure t
  :hook (projectile-mode)
  :bind (("C-c p f" . helm-projectile))
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      25)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'extended)
    (set-face-attribute 'hl-line nil :background "#333333"))
  :bind
  (:map global-map
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ("M-n f t"    . treemacs)
        ("M-n f B"    . treemacs-bookmark)
        ("M-n f C-t"  . treemacs-find-file)
        ("M-n f M-t"  . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs
  :ensure t)

(use-package rainbow-delimiters ;; colorize (), {}, []
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers ;; programming identifiers get consistent colors (helps spot typos)
  :ensure t
  :hook (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-mode ;; visualize color strings like 'blue'
  :ensure t
  :hook (css-mode web-mode prog-mode))

(use-package direnv
  :ensure t
  :hook ((clojure-mode . direnv-mode))
  :config
  (setq direnv--installed "/usr/local/bin/direnv"))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode))

(defun personal-sp-web-mode-is-code-context (id action context)
  "Determines whether we're in a code context for Smartparens.
ID - ignored
ACTION - the smartparens action
CONTEXT - ignored"
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook ((eshell-mode emacs-lisp-mode clojure-mode cider-mode cider-repl-mode)
         . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair '(html-mode sgml-mode web-mode) "<" ">")
  (sp-local-pair '(html-mode sgml-mode web-mode) "<" nil
                 :when '(personal-sp-web-mode-is-code-context))

  (define-key smartparens-mode-map (kbd "M-\\")  'personal-delete-horizontal-space)
  (define-key smartparens-mode-map (kbd "M-SPC") 'personal-just-one-space)

  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)

  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)

  (define-key smartparens-mode-map (kbd "s-a")   'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "s-e")   'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  ;; note the uppercase "D", also bound to M-s
  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

  ;; this "<delete>" is the one near the "home" button on larger keyboards (104 vs 88)
  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)

  ;; Mac "<backspace>" is labeled delete
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
  ;; end

  ;; to work these backwards, prefix with C-- and C-M--
  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp))

(defun personal-just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun personal-delete-horizontal-space ()
  "Command to delete all whitespace."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(provide 'romney-coding)
;;; romney-coding.el ends here
