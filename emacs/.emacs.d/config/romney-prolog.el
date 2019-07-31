;;; romney-prolog.el --- Prolog programming
;;; Commentary:
;;
;;; Code:

(use-package ediprolog
  :ensure t
  :defer t
  :bind ("C-c C-p" . ediprolog-dwim)
  :mode (("\\.pl\\'" . prolog-mode)
         ("\\.prolog\\'" . prolog-mode)))

(provide 'romney-prolog)
;;; romney-prolog.el ends here
