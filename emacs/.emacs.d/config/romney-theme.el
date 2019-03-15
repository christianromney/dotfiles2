;;; romney-theme.el --- User interface and themeing
;;; Commentary:
;;
;;; Code:
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; -- font handling --
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 180
                    :weight 'regular)

(set-face-attribute 'mode-line nil
                    :family "Iosevka"
                    :height 160
                    :weight 'thin)

(set-face-attribute 'variable-pitch nil
                    :family "Big Caslon Medium")

(copy-face 'default 'fixed-pitch)

(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

(winner-mode)
(column-number-mode)                                ;; enable column numbers
(line-number-mode)                                  ;; enable line numbers
(size-indication-mode)                              ;; enable file sizes
(global-auto-revert-mode)                           ;; revert buffers when files changed externally
(blink-cursor-mode -1)
(global-prettify-symbols-mode +1)
(global-hl-line-mode +1)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(setq-default fill-column 80)                       ;; fill column at 80 chars
(setq-default indent-tabs-mode nil)            ;; don't use tabs to indent
(setq-default tab-width 2)                          ;; don't waste real estate needlessly

(use-package powerline
  :defer nil
  :ensure t)

(use-package spaceline
  :defer nil
  :ensure t
  :config
  (require 'powerline)
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  ;; -- disabled --
  (spaceline-toggle-auto-compile-off)
  (spaceline-toggle-battery-off)
  (spaceline-toggle-column-off)
  (spaceline-toggle-minor-modes-off)
  ;; -- enabled --
  (spaceline-toggle-anzu-on)
  (spaceline-toggle-buffer-modified-on)
  (spaceline-toggle-buffer-position-on)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-buffer-size-on)
  (spaceline-toggle-flycheck-error-on)
  (spaceline-toggle-flycheck-info-on)
  (spaceline-toggle-flycheck-warning-on)
  (spaceline-toggle-helm-buffer-id-on)
  (spaceline-toggle-helm-help-on)
  (spaceline-toggle-hud-on)
  (spaceline-toggle-line-column-on)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-selection-info-on))

(use-package dracula-theme
  :defer nil
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package ov ;; easy overlays
  :defer t
  :ensure t
  :config
  (require 'ov))

(use-package linum
  :defer nil
  :ensure t
  :config
  (setq linum-format " %d ")
  (global-linum-mode t))

(use-package hlinum
  :defer nil
  :ensure t
  :config
  (hlinum-activate))

;; Highlights 'TODO' and similar keywords in comments and strings.
(use-package hl-todo
  :defer t
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

(provide 'romney-theme)
;;; romney-theme.el ends here
