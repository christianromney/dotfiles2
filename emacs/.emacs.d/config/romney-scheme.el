;;; romney-scheme.el --- Scheme/Racket programming
;;; Commentary:
;;
;;; Code:

(use-package geiser
  :ensure t
  :defer t
  :config
  (setq geiser-active-implementations '(racket)))

(provide 'romney-scheme)
;;; romney-scheme.el ends here
