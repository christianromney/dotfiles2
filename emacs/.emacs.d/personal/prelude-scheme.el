;; Geiser settings
(setq geiser-active-implementations '(racket))
(setq geiser-repl-startup-time 10000)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
(setq geiser-repl-query-on-kill-p nil)
(setq geiser-implementations-alist
      '(((regexp "\\.scm$") racket)
        ((regexp "\\.ss$") racket)
        ((regexp "\\.rkt$") racket)))

(setq geiser-mode-smart-tab-p t)

;; (require 'ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'geiser-repl-mode))

;; (require 'ob-scheme)

;; Easy way to launch geiser
(global-set-key (kbd "C-c M-g") 'run-geiser)
