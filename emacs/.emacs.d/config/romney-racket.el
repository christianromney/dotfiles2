;;; romney-racket.el --- Racket programming
;;; Commentary:
;;
;;; Code:
(use-package racket-mode
  :ensure t
  :defer t
  :mode (("\\.ss\\'"    . racket-mode)
         ("\\.rkt\\'"   . racket-mode)
         ("\\.pie\\'"   . racket-mode)
         ("\\.scrbl\\'" . racket-mode))
  :bind (:map racket-mode-map ("C-c r". racket-run))
  :config
  (add-hook 'racket-mode-hook      #'smartparens-strict-mode)
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (setq racket-command-timeout 3)
  ;; pie language indentation
  (put 'claim 'racket-indent-function 'defun)
  (put 'which-Nat 'racket-indent-function 1)
  (put 'iter-Nat 'racket-indent-function 1)
  (put 'ind-Nat 'racket-indent-function 1)
  (put 'rec-Nat'racket-indent-function 1)
  (put 'rec-List 'racket-indent-function 1)
  (add-to-list 'racket-keywords "claim"))

(provide 'romney-racket)
;;; romney-racket.el ends here
