;;; my/prelude-appearance --- Visual / Appearance Configuration for Emacs
;;; 
;;; Commentary:
;;; Tabs, whitespace, mode-line, line-number,
;;; theme and font settings all live here
;;;
;;; Code:
;;;
;; Whitespace, Indentation
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default sgml-basic-offset 2)
(setq-default indent-tabs-mode nil) ;; spaces

(setq prelude-whitespace nil)
(setq sentence-end-double-space nil)

;; Highlights, Parens
(setq global-hl-line-mode nil)
(setq visible-bell nil)
(setq load-prefer-newer t)
(setq diff-switches "-u")

;;; Themes and Global Minor Modes
(use-package flatland-theme
  :init
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-18"))
  (disable-theme 'zenburn)
  (disable-theme 'solarized)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (whitespace-mode t)
  (global-prettify-symbols-mode t)
  :config
  (load-theme 'flatland)
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;; Mode Line
(use-package powerline
  :config
  (powerline-center-theme)
  (custom-set-faces
   '(powerline-active1 ((t (:foreground "#e0e0e0" :background "#202320" ))))
   '(powerline-active2 ((t (:foreground "#b9d977" :background "#353a3d" ))))))

;;; Line Numbers
(use-package linum
  :init
  (setq linum-format " %d ")
  :config
  (global-linum-mode t))

(use-package ansi-color
  :config
  (ansi-color-for-comint-mode-on))

(diminish 'company-mode " ©")
(diminish 'flycheck-mode " ✓")
(diminish 'whitespace-mode " Ws")
(diminish 'helm-mode)
(diminish 'prelude-mode)

(provide 'personal/prelude-appearance)
;;; prelude-appearance.el ends here
