;;; Whitespace, Indentation
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default sgml-basic-offset 2)
(setq-default indent-tabs-mode nil)

;; Highlights, Parens, Spelling
(setq prelude-whitespace nil
      sentence-end-double-space nil
      prelude-flyspell nil
      prelude-guru nil
      visible-bell nil
      diff-switches "-u")

(add-to-list 'default-frame-alist '(font . "Source Code Pro-20"))

;;; Themes and Global Minor Modes
(use-package smyx-theme
  :ensure t
  :diminish (projectile-mode . " Prj") (company-mode . " Cmp") (flycheck-mode . " ✓") prelude-mode
  :config
  (disable-theme 'zenburn)
  (disable-theme 'solarized)
  (load-theme 'smyx)
  (menu-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (whitespace-mode t)
  (global-prettify-symbols-mode t)
  (global-auto-revert-mode t)
  (global-hl-line-mode +1)
  (global-flycheck-mode -1)
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;; Mode Line
(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

;;; Line Numbers
(use-package linum
  :ensure t
  :config
  (setq linum-format " %d ")
  (global-linum-mode t))

(use-package ansi-color
  :ensure t
  :config
  (ansi-color-for-comint-mode-on))

;; Colors for dired
(use-package diredful
  :ensure t
  :config
  (diredful-mode 1))

(provide 'prelude-appearance)
