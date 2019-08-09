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
                    :height 200
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
(global-prettify-symbols-mode +1)

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

(use-package doom-themes
  :ensure t
  :defer nil
  :config
  (load-theme 'doom-tomorrow-night t)
  (global-hl-line-mode +1)
  (set-cursor-color "#ffffff")
  (set-face-attribute 'region nil
                      :background "#fbf3ac"
                      :foreground "#000000")
  (blink-cursor-mode +1)
  (setq-default blink-cursor-blinks -1 )
  (setq-default cursor-type '(hollow . 1)) ;; box, hollow, bar, hbar
  (setq x-stretch-cursor t
        fancy-splash-image nil ;; (expand-file-name "martell.png" personal-config-dir)
        fancy-about-text nil
        fancy-startup-text nil
        inhibit-splash-screen t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-checker-simple-format t
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-env-version nil
        doom-modeline-lsp nil
        doom-modeline-github t
        doom-modeline-github-interval (* 30 60)
        inhibit-compacting-font-caches t)
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts))
  (add-to-list 'all-the-icons-icon-alist
             '("\\.p[ml]$"
               all-the-icons-alltheicon "prolog"
               :height 1.1
               :face all-the-icons-lmaroon)))

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
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

(provide 'romney-theme)
;;; romney-theme.el ends here
