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

(provide 'personal/prelude-scheme)
