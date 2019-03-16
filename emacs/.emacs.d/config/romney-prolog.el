;;; romney-prolog.el --- Prolog programming
;;; Commentary:
;;
;;; Code:

(use-package ediprolog
  :ensure t
  :defer t
  :bind ("<f9>" . ediprolog-dwim)
  :mode ("\\.pro\\'" . prolog-mode))

(provide 'romney-prolog)
;;; romney-prolog.el ends here
