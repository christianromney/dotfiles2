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
(setq-default fish-indent-offset 2)

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-x g" . magit-status))
  :init
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(use-package magit-todos
  :ensure t
  :defer t
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :ensure t
  :defer t)

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :custom-face
  (git-timemachine-minibuffer-author-face
   ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face
   ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

(use-package git-messenger
  :ensure t
  :defer t
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t
        git-messenger:show-detail t))

(use-package smerge-mode
  :ensure nil
  :diminish
  :commands (smerge-mode
             smerge-auto-leave
             smerge-next
             smerge-prev
             smerge-keep-base
             smerge-keep-upper
             smerge-keep-lower
             smerge-keep-all
             smerge-keep-current
             smerge-keep-current
             smerge-diff-base-upper
             smerge-diff-upper-lower
             smerge-diff-base-lower
             smerge-refine
             smerge-ediff
             smerge-combine-with-next
             smerge-resolve
             smerge-kill-current)
  :preface
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-hydra/body))))))

(use-package browse-at-remote
  :ensure t
  :defer t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :defer t
  :defines (helm-completion-system)
  :diminish projectile-mode
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" personal-data-dir))
  (projectile-mode +1))

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

(use-package highlight-numbers
  :ensure t
  :defer t
  :hook (prog-mode . highlight-numbers-mode))

(use-package rainbow-delimiters ;; colorize (), {}, []
  :ensure t
  :pin melpa-stable
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers ;; programming identifiers get consistent colors (helps spot typos)
  :ensure t)

(use-package rainbow-mode ;; visualize color strings like 'blue'
  :ensure t
  :hook (css-mode web-mode prog-mode))

(use-package direnv
  :ensure t
  :hook ((prog-mode . direnv-mode))
  :config
  (setq direnv--installed "/usr/local/bin/direnv"))

(use-package fish-mode
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode))

(use-package sql
  :defer t
  :bind (("C-c M-p" . sql-postgres)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

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
