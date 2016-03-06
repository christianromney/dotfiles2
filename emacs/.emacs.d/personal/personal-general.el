;; UTF-8 everywhere
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; General Emacs Settings
(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      load-prefer-newer t
      column-number-mode t
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t
      vc-follow-symlinks t
      emerge-diff-options "--ignore-all-space"
      echo-keystrokes 0.1
      initial-scratch-message nil
      dired-use-ls-dired nil
      make-backup-files nil
      indent-tabs-mode nil
      tab-always-indent 'complete
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      cua-enable-cua-keys nil
      default-input-method "MacOSX"
      system-name (car (split-string system-name "\\.")))

;; search with ack (including from helm)
(use-package ack
  :ensure t)

;; use helm everywhere possible
(use-package helm
  :ensure t
  :bind
  ("C-c M-i" . helm-multi-swoop))

(use-package helm-projectile
  :ensure t)

(require 'prelude-helm)
(require 'prelude-helm-everywhere)

(when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
(helm-autoresize-mode t)
(setq helm-split-window-in-side-p           t 
      helm-M-x-fuzzy-match                  t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-locate-fuzzy-match               t
      helm-apropos-fuzzy-match              t
      helm-move-to-line-cycle-in-source     t
      helm-lisp-fuzzy-completion            t
      helm-ff-search-library-in-sexp        t 
      helm-scroll-amount                    8 
      helm-ff-file-name-history-use-recentf t
      helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
      helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f")

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

;; documentation popup alonside company completion candidates
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

;;; APPEARANCE, INDENTATION, THEMEING,
;;
;; Whitespace, Indentation
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

;; Adobe Source Code Pro font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-20"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; modeline appearance
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode +1))

(use-package powerline
  :ensure t
  :config
  (require 'powerline))

;;; Theme settings and built-in global minor modes
(use-package moe-theme
  :ensure t
  :diminish projectile-mode company-mode (flycheck-mode . " ✓") prelude-mode helm-mode
  :config
  (require 'moe-theme)
  (disable-theme 'zenburn)
  (disable-theme 'solarized)
  (load-theme 'moe-dark)
  (moe-theme-set-color 'red)
  (powerline-moe-theme)
  (setq moe-theme-highlight-buffer-id t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode t)
  (column-number-mode t)
  (whitespace-mode t)
  (global-prettify-symbols-mode t)
  (global-auto-revert-mode)
  (global-hl-line-mode -1)
  (global-flycheck-mode -1))

(use-package rainbow-identifiers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;; Line Numbers
(use-package linum
  :ensure t
  :config
  (setq linum-format " %d ")
  (global-linum-mode t))

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))


;; dired enhancements
(use-package dired+
  :ensure t)

(use-package diredful
  :ensure t
  :config
  (require 'dired-x)
  (diredful-mode +1))

;; info mode
(use-package info+
  :ensure t)

(use-package git-gutter-fringe+
  :ensure t)

;; Newsticker
(add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
(setq newsticker-url-list
      '(("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil)
        ("xkcd" "https://www.xkcd.com/rss.xml" nil nil nil)
        ("emacs rocks" "http://emacsrocks.com/atom.xml" nil nil nil)
        ("endlessparentheses" "http://endlessparentheses.com/atom.xml" nil nil nil)
        ("Daring Fireball" "https://daringfireball.net/feeds/main" nil nil nil)
        ("BBC News" "http://feeds.bbci.co.uk/news/world/rss.xml" nil nil nil)
        ("CNN News" "http://rss.cnn.com/rss/cnn_topstories.rss" nil nil nil)
        ("NCAAB News" "http://sports.espn.go.com/espn/rss/ncb/news" nil nil nil)))

;; Thesauruse
(use-package synosaurus
  :ensure t
  :defer t
  :bind
  ("C-x t" . synosaurus-choose-and-replace))

;; Recent files listed in helm-buffers-list
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25))

;; Better package manager
(use-package paradox
  :ensure t
  :defer t
  :bind
  ("<f2>" . paradox-list-packages))

;; Spelling
(use-package flyspell-mode
  :diminish (flyspell-mode . " FSp")
  :config
  (setq flyspell-issue-welcome-flag nil
        ispell-program-name "/usr/local/bin/aspell"
        ispell-extra-args '("--sug-mode=ultra")
        ispell-list-command "list"))

;; Jumping / Navigating
(use-package ace-jump-buffer
  :ensure t
  :bind
  ("C-c J" . ace-jump-buffer))

(use-package ace-window
  :ensure t
  :defer t
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-c H" . ace-window))

(use-package ace-isearch
  :ensure t
  :defer t
  :config
  (global-ace-isearch-mode 1))

(use-package ace-jump-zap
  :ensure t
  :defer t
  :bind
  (("M-z" . ace-jump-zap-up-to-char-dwim)
   ("M-Z" . ace-jump-zap-to-char-dwim)))

;; ASCII doc
(use-package adoc-mode
  :ensure t
  :defer t
  :config
  (setq auto-mode-alist (cons '("\\.adoc$"  . adoc-mode) auto-mode-alist)))
