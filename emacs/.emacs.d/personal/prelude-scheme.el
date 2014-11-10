;; Geiser settings
(setq geiser-active-implementations '(guile racket))
(setq geiser-repl-startup-time 10000)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-implementations-alist
      '(((regexp "\\.scm$") guile)
        ((regexp "\\.ss$") guile)
        ((regexp "\\.rkt$") racket)))

(setq geiser-mode-smart-tab-p t)

;; Easy way to launch geiser
(global-set-key (kbd "C-c M-g") 'run-geiser)
