;;; romney-helm.el --- everything helm
;;; Commentary:
;;;
;;; This file should be loaded after romney-general.el
;;;
;;; Code:
(use-package helm
  :ensure t
  :pin melpa-stable
  :diminish helm-mode
  :defines (helm-split-window-in-side-p
            helm-M-x-fuzzy-match
            helm-buffers-fuzzy-matching
            helm-recentf-fuzzy-match
            helm-semantic-fuzzy-match
            helm-imenu-fuzzy-match
            helm-locate-fuzzy-match
            helm-apropos-fuzzy-match
            helm-lisp-fuzzy-completion
            helm-ff-search-library-in-sexp
            helm-ff-file-name-history-use-recentf
            helm-command-map
            helm-grep-default-command
            helm-grep-default-recurse-command)
  :bind
  (("M-x"      . helm-M-x)
   ("C-x C-m"  . helm-M-x)
   ("C-c h"    . helm-command-prefix)
   ("M-i"      . helm-semantic-or-imenu)
   ("M-y"      . helm-show-kill-ring)
   ("C-x b"    . helm-mini)
   ("C-x C-b"  . helm-buffers-list)
   ("C-x C-f"  . helm-find-files)
   ("C-x r b"  . helm-filtered-bookmarks)
   ("C-h f"    . helm-apropos)
   ("C-h r"    . helm-info-emacs)
   ("C-h C-l"  . helm-locate-library)
   :map helm-map
   ("<tab>"     . helm-execute-persistent-action)
   ("C-i"       . helm-execute-persistent-action)
   ("C-z"       . helm-select-action))
  :config
  (require 'helm)
  (require 'helm-config)
  (setq helm-split-window-in-side-p           t
        helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t
        helm-imenu-fuzzy-match                t
        helm-locate-fuzzy-match               t
        helm-apropos-fuzzy-match              t
        helm-move-to-line-cycle-in-source     t
        helm-lisp-fuzzy-completion            t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-autoresize-mode t
        helm-autoresize-max-height            50
        helm-autoresize-min-height            20
        helm-follow-mode-persistent           t
        helm-google-suggest-use-curl-p        t
        helm-grep-default-command             "ack -Hn -i --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command     "ack -H -i --no-group --no-color %e %p %f")
  (substitute-key-definition 'xref-find-definitions 'helm-etags-select global-map)

  (helm-mode +1)

  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (romney/focus-command-windows '(helm-man-woman))

  (when (executable-find "rg")
    (setq helm-grep-ag-command
          "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches
          '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))))

(use-package helm-themes
  :ensure t
  :defer t
  :bind ("C-c C-t" . helm-themes))

(use-package helm-core
  :ensure t
  :pin melpa-stable
  :defer t)

(use-package helm-descbinds
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :ensure t
  :hook projectile-mode
  :bind (("C-c p f" . helm-projectile-find-file-dwim)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p h" . helm-projectile)
         ("C-c p F" . helm-projectile-find-file-in-known-projects)
         ("C-c p d" . helm-projectile-find-dir)
         ("C-c p e" . helm-projectile-recentf)
         ("C-c p a" . helm-projectile-find-other-file)
         ("C-c p b" . helm-projectile-switch-to-buffer)
         ;;("C-c p s g" . helm-projectile-grep)
         ;;("C-c p s a" . helm-projectile-ack)
         ;;("C-c p s s" . helm-projectile-ag)
         )
  :config
  (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)

  (helm-projectile-on))

(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell helm)
  :config (require 'flyspell-correct-helm))

(provide 'romney-helm)
;;; romney-helm.el ends here
