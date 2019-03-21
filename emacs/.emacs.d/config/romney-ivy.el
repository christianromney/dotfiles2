;;; romney-ivy.el --- Ivy, counsel, swiper for search
;;; Commentary:
;;;
;;; This file should be loaded after romney-general.el
;;;
;;; Code:
(use-package flx :ensure t)
(use-package counsel ;; counsel (ivy + swiper are transitive deps)
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-rg)
         ("C-c G" . counsel-git-grep)
         ("C-c T" . counsel-load-theme)
         ("C-c C-i " . counsel-imenu)
         ("C-c C-r" . ivy-resume)
         ("C-c C-b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         :map ivy-minibuffer-map
         ("C-m" . ivy-alt-done)
         ("C-l" . counsel-up-directory) ;; like Helm
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-height 20
        ivy-wrap t
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-extra-directories nil
        magit-completing-read-function 'ivy-completing-read
        projectile-completion-system 'ivy
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (ivy-mode 1))

(use-package ivy-xref
  :ensure t
  :defer t
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  (setq ivy-format-function #'ivy-format-function-line))

(use-package counsel-projectile
  :ensure t
  :hook projectile-mode
  :bind (("C-c p" . projectile-command-map))
  :config
  (counsel-projectile-mode t))

(use-package counsel-etags
  :ensure t
  :defer t
  :bind (("M-." . counsel-etags-find-tag-at-point)
         ("C-c t" . counsel-etags-list-tag))
  :config
  (setq tags-revert-without-query t
        large-file-warning-threshold nil)
  (add-to-list 'counsel-etags-ignore-directories "target")
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local))

(use-package counsel-osx-app
  :ensure t
  :defer t
  :bind ("C-c O" . counsel-osx-app))

(use-package flyspell-correct-ivy
  :ensure t
  :defer t
  :bind ("C-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'romney-ivy)
;;; romney-ivy.el ends here
