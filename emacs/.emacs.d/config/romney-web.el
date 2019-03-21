;;; romney-web.el --- Web development packages
;;; Commentary:
;;
;;; Code:
(use-package web-mode
  :ensure t
  :defer t
  :defines (web-mode-enable-auto-pairing)
  :diminish web-mode
  :mode (("\\.jsp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config
  ;; make web-mode play nice with smartparens
  (setq web-mode-enable-auto-pairing nil)
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(use-package markdown-mode
  :ensure t
  :defer t
  :ensure-system-package multimarkdown
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook #'subword-mode))

(use-package scss-mode
  :ensure t
  :defer t
  :defines (scss-compile-at-save)
  :config
  (setq scss-compile-at-save nil))

(provide 'romney-web)
;;; romney-web.el ends here
