;;; romney-racket.el --- Racket programming
;;; Commentary:
;;
;;; Code:
(use-package racket-mode
  :ensure t
  :defer t
  :mode ("\\.pie\\'" . racket-mode)
  :bind (:map racket-mode-map
         ("C-c r". racket-run)))

(provide 'romney-racket)
;;; romney-racket.el ends here
