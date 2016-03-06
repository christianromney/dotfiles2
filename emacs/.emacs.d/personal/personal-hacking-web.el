;; Zen Coding 
(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook 'yas-minor-mode-on))

(use-package less-css-mode
  :ensure t
  :defer t)

(require 'prelude-css)
(require 'prelude-scss)
(require 'prelude-web) ;; web-mode 
