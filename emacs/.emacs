;;; .emacs --- Summary
;;; My emacs configuration
;;;
;;; Commentary:
;;; I've cherry-picked much of this configuration from various
;;; people around the web including, Bozhidar Batsov and Sascha Chua. Some of
;;; the code is inspired by Prelude, but this configuration is much slimmer and
;;; loads faster than Prelude and some of my previous configurations.
;;;
;;; Code:
;;; --- personal info ---

(setq user-email-address "cromney@pointslope.com"
      user-full-name "Christian Romney"
      calendar-latitude 26.017
      calendar-longitude -80.34110799999999
      calendar-location-name "Pembroke Pines, FL")

;;; --- startup settings ---
(setq inhibit-startup-message t              ;; no initial message in scratch buffer
      scroll-margin 0                        ;; scroll settings
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      package-enable-at-startup nil)         ;; don't enable packages unless I tell you

(setq-default initial-major-mode (quote emacs-lisp-mode))
(setq-default initial-scratch-message nil)

(add-to-list 'default-frame-alist '(font . "Sauce Code Powerline-20"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; --- save directories ---

(defun personal/ensure-dir (dir)
  "Create a directory (given as DIR) if it doesn't already exist."
  (unless (file-exists-p dir)
  (make-directory dir)))

(defconst personal-savefile-dir
  (expand-file-name "savefile" user-emacs-directory))

(defconst personal-backup-dir
  (expand-file-name "backups" personal-savefile-dir))

(defconst personal-autosave-dir
  (expand-file-name "autosave" personal-savefile-dir))

(defconst personal-desktop-dir
  (expand-file-name "desktop" personal-savefile-dir))

(personal/ensure-dir personal-savefile-dir)
(personal/ensure-dir personal-backup-dir)
(personal/ensure-dir personal-autosave-dir)
(personal/ensure-dir personal-desktop-dir)

;; --- package configuration ---

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("billpiel" . "http://billpiel.com/emacs-packages/") t)

(setq package-pinned-packages
      '((clojure-mode        . "melpa-stable")
        (cider               . "melpa-stable")
        (clj-refactor        . "melpa-stable")
        (company             . "melpa-stable")
        (helm                . "melpa-stable")
        (helm-ag             . "melpa-stable")
        (helm-core           . "melpa-stable")
        (helm-descbinds      . "melpa-stable")
        (projectile          . "melpa-stable")
        (projectile-ruby     . "melpa-stable")
        (rainbow-delimiters  . "melpa-stable")
        (ggtags              . "melpa-stable")))

(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)

(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))

;; -- general settings --

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill)

  ;; prevent emacs error due to macos' ls not supporting long args...
  ;; ls does not support --dired; see ‘dired-use-ls-dired’
  ;; requires "brew install coreutils"
  (let ((ls-program "/usr/local/bin/gls"))
    (when (file-exists-p ls-program)
      (setq insert-directory-program ls-program))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq load-prefer-newer t                    ;; load latest bytecode
      gc-cons-threshold 50000000             ;; wait till 50MB to GC
      large-file-warning-threshold 100000000 ;; warn if file exceeds 100MB
      tab-always-indent 'complete            ;; tab completes
      indent-tabs-mode nil                   ;; don't use tabs
      require-final-newline t                ;; file ends in newline
      default-input-method "MacOSX"          ;; macOS comfort
      confirm-nonexistent-file-or-buffer nil ;; don't annoy me with questions
      echo-keystrokes 0.001                  ;; hints show up in echo area faster
      make-backup-files nil                  ;; don't pollute the filesystem
      next-error-highlight t                 ;; highlight until next command/location
      next-error-highlight-no-select t       ;; highlight indefinitely until replaced
      query-replace-highlight t              ;; highlight matches during query replace
      sentence-end-double-space nil          ;; this was always stupid
      shift-select-mode nil                  ;; don't mess with the mark
      transient-mark-mode t                  ;; regions are temporary like most apps
      truncate-partial-width-windows nil     ;; respect value of 'truncate-lines' variable
      vc-follow-symlinks t                   ;; symlinks aren't second-class citizens
      max-specpdl-size 2400                  ;; limit on number of variable bindings
      sh-learn-basic-offset t                ;; try to figure out offset for shell mode
      locale-coding-system 'utf-8            ;; utf-8 character encoding
      )

;;; --- keep current search result in the center of the screen ---

(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

(defadvice
    isearch-forward
    (after isearch-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-forward)

(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-backward)

;;; --- character encoding ---

(prefer-coding-system        'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;;; --- built-in features ---

(require 'subword)
(global-subword-mode +1)
(define-key subword-mode-map (vector 'remap 'transpose-words) nil)
(define-key subword-mode-map (vector 'remap 'upcase-word) nil)
(define-key subword-mode-map (vector 'remap 'downcase-word) nil)

(require 'misc)
(global-set-key (kbd "M-Z")     'zap-up-to-char)
(global-set-key (kbd "C-x f")   'find-file)         ;; typo elimination
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x \\")  #'align-regexp)

(require 'tramp)
(setq tramp-default-method "ssh") ;; keep in mind known issues with zsh - see emacs wiki

(require 'which-func)
(which-function-mode 1) ;; shows applicable key bindings

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" personal-savefile-dir)
      bookmark-save-flag 1)

(require 'midnight) ;; clean up obsolete buffers automatically

;; saner regexp syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'compile)
(setq compilation-ask-about-save nil          ;; Just save before compiling
      compilation-always-kill t               ;; Just kill old compile processes before
      compilation-scroll-output 'first-error) ;; Automatically scroll to first error

;;; --- enable various 'off by default' features ---

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)

;;; --- expansions ---

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;;; --- appearance ---

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(winner-mode)
(column-number-mode)                   ;; enable column numbers
(line-number-mode)                     ;; enable line numbers
(size-indication-mode)                 ;; enable file sizes
(delete-selection-mode)                ;; delete selections with a keypress
(global-auto-revert-mode)              ;; revert buffers when files changed externally

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(setq-default fill-column 80)                  ;; fill column at 80 chars
(setq-default indent-tabs-mode nil)            ;; don't use tabs to indent
(setq-default tab-width 2)                     ;; don't waste real estate needlessly
(setq-default c-basic-offset 2)                ;; everything should indent w/ 2 spaces
(setq-default sh-basic-offset 2)
(setq-default elixir-basic-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default sgml-basic-offset 2)

(use-package all-the-icons
  :ensure t)

(use-package powerline
  :ensure t
  :defer t)

(use-package spaceline
  :ensure t
  :init
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
  :ensure t
  :defer t
  :init (load-theme 'dracula t))

(blink-cursor-mode -1)
(global-prettify-symbols-mode +1)
(global-hl-line-mode +1)

;;; --- custom functions ---

(defun personal-delete-horizontal-space ()
  "Command to delete all whitespace."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(defun personal-just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun personal-sp-web-mode-is-code-context (id action context)
  "Determines whether we're in a code context for Smartparens.
ID - ignored
ACTION - the smartparens action
CONTEXT - ignored"
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

;;; --- additional packages ---

(use-package bookmark+
  :ensure t
  :defer t)

(use-package ov ;; easy overlays
  :ensure t
  :config
  (require 'ov))

(use-package linum
  :ensure t
  :config
  (setq linum-format " %d ")
  (global-linum-mode t))

(use-package hlinum
  :ensure t
  :config
  (hlinum-activate))

(use-package avy ;; avy is a better ace-jump-mode
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char))
  :config
  (setq avy-background t
        avy-style 'at-full)
  (avy-setup-default))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package gist
  :ensure t
  :defer t)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package direnv
  :ensure t
  :config
  (setq direnv--installed "/usr/local/bin/direnv"))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind (("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :config
  (require 'projectile)
  (setq projectile-cache-file (expand-file-name  "projectile.cache" personal-savefile-dir))
  (projectile-mode t))

;; (use-package pt ;; platinum searcher (think "ack" but faster; written in Go)
;;   :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  (add-hook 'text-mode-hook 'abbrev-mode))

(use-package paren ;; show matching parens
  :config
  (show-paren-mode +1))

(require 'saveplace) ;; remember location when saving files
(use-package saveplace
  :config
  (setq save-place-file
        (expand-file-name "saveplace" personal-savefile-dir))
  (setq-default save-place t))

;; > Autosave Work <
(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file
        (expand-file-name "savehist" personal-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file
        (expand-file-name "recentf" personal-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  (windmove-default-keybindings)) ;; use shift + arrow keys to nav windows

(use-package dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (require 'dired-x))

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package anzu ;; search & replace match info e.g. 1 of N
  :ensure t
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill ;; copy with shortcuts
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell ;; find programs on shell $PATH
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text ;; move text blocks around
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package rainbow-delimiters ;; colorize (), {}, []
  :ensure t
  :diminish rainbow-delimiters-mode)

(use-package rainbow-identifiers ;; programming identifiers get consistent colors (helps spot typos)
  :ensure t
  :diminish rainbow-identifiers-mode)

(use-package rainbow-mode ;; visualize color strings like 'blue'
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-faces nil
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" personal-savefile-dir)
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1)
  (ido-everywhere +1))

;; formerly known as ido-ubiquitous-mode
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode +1))

(use-package smex ;; smarter M-x
  :ensure t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" personal-savefile-dir))
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package company ;; COMPlete ANYthing
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (global-company-mode 1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package pos-tip
  :ensure t)

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (require 'helm)
  (require 'helm-config)
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
        helm-autoresize-max-height            100
        helm-autoresize-min-height            20
        helm-follow-mode-persistent           t
        helm-grep-default-command             "ack -Hn -i --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command     "ack -H -i --no-group --no-color %e %p %f")

  ;; (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (substitute-key-definition 'xref-find-definitions 'helm-etags-select global-map)
  (helm-autoresize-mode t)
  (helm-mode +1)

  (global-unset-key (kbd "C-x c"))
  (global-set-key   (kbd "C-c h")      'helm-command-prefix)
  (global-set-key   (kbd "C-c h o")    'helm-occur)
  (global-set-key   (kbd "M-x")        'helm-M-x)
  (global-set-key   (kbd "C-x C-m")    'helm-M-x)
  (global-set-key   (kbd "M-i")        'helm-imenu)
  (global-set-key   (kbd "M-y")        'helm-show-kill-ring)
  (global-set-key   (kbd "C-x b")      'helm-mini)
  (global-set-key   (kbd "C-x C-b")    'helm-buffers-list)
  (global-set-key   (kbd "C-x C-f")    'helm-find-files)
  (global-set-key   (kbd "C-h f")      'helm-apropos)
  (global-set-key   (kbd "C-h r")      'helm-info-emacs)
  (global-set-key   (kbd "C-h C-l")    'helm-locate-library)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")   'helm-select-action))

(use-package helm-ag
  :ensure t)

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  (global-set-key (kbd "C-c p s a") 'helm-projectile-ack))

(use-package super-save ;; save buffers on lost focus
  :ensure t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package crux ;; misc useful utils from Prelude
  :ensure t
  :config
  (require 'rect)
  (crux-with-region-or-line kill-region)
  :bind
  (("C-c o"                        . crux-open-with)
   ("M-O"                          . crux-smart-open-line)
   ("C-c n"                        . crux-cleanup-buffer-or-region)
   ("C-c f"                        . crux-recentf-ido-find-file)
   ("C-M-z"                        . crux-indent-defun)
   ("C-c u"                        . crux-view-url)
   ("C-c e"                        . crux-eval-and-replace)
   ("C-c w"                        . crux-swap-windows)
   ("C-c D"                        . crux-delete-file-and-buffer)
   ("C-c r"                        . crux-rename-buffer-and-file)
   ("C-c t"                        . crux-visit-term-buffer)
   ("C-c k"                        . crux-kill-other-buffers)
   ("C-c TAB"                      . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c I"                        . crux-find-user-init-file)
   ("C-c S"                        . crux-find-shell-init-file)
   ("C-c s"                        . crux-ispell-word-then-abbrev)
   ("s-r"                          . crux-recentf-ido-find-file)
   ("s-j"                          . crux-top-join-line)
   ("C-^"                          . crux-top-join-line)
   ("s-k"                          . crux-kill-whole-line)
   ("C-<backspace>"                . crux-kill-line-backwards)
   ("s-o"                          . crux-smart-open-line-above)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([(shift return)]               . crux-smart-open-line)
   ([(control shift return)]       . crux-smart-open-line-above)
   ([remap kill-whole-line]        . crux-kill-whole-line)))

(use-package undo-tree ;; better undo / redo
  :ensure t
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package which-key ;; help remember keybindings
  :ensure t
  :config
  (which-key-mode +1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flyspell-correct-helm
  :ensure t)

(use-package flyspell
  :ensure t
  :config
  (require 'flyspell-correct-helm)
  :bind
  (("C-;" . flyspell-correct-previous-word-generic)))

;;; --- built-in programming modes ---

;; shell
(require 'sh-script)
(defvar prelude-prezto-files
  '("zlogin" "zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      prelude-prezto-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) prelude-prezto-files))
                (sh-set-shell "zsh"))))

;; xml
(require 'nxml-mode)
(push '("<\\?xml" . nxml-mode) magic-mode-alist)
(add-to-list 'auto-mode-alist '("\\.pom$" . nxml-mode))
(setq nxml-child-indent 2)
(setq nxml-attribute-indent 2)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)

;; css
(eval-after-load 'css-mode
  '(progn
     (defun personal-css-mode-hook ()
       (rainbow-mode +1))
     (add-hook 'css-mode-hook (lambda () (run-hooks 'personal-css-mode-hook)))))

;; -- programming modes --

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'web-mode "<" nil :when '(personal-sp-web-mode-is-code-context))
  (sp-with-modes '(html-mode sgml-mode web-mode) (sp-local-pair "<" ">"))
  (define-key smartparens-mode-map (kbd "M-\\") 'personal-delete-horizontal-space)
  (define-key smartparens-mode-map (kbd "M-SPC") 'personal-just-one-space)

  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)

  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)

  (define-key smartparens-mode-map (kbd "s-a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "s-e") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  ;; note the uppercase "D", also bound to M-s
  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

  ;; this "<delete>" is the one near the "home" button on larger keyboards (104 vs 88)
  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)

  ;; Mac "<backspace>" is labeled delete
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
  ;; end

  ;; to work these backwards, prefix with C-- and C-M--
  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp))

(use-package web-mode
  :ensure t
  :diminish (web-mode . " web ")
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist
               '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

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
  :ensure t)

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'whitespace-mode)
  (add-hook 'yaml-mode-hook 'subword-mode))

(use-package inf-ruby
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc")
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-tools
  :ensure t
  :defer t)

(use-package yari
  :ensure t
  :defer t)

(use-package ruby-mode
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  (eval-after-load 'ruby-mode
    '(progn
       (defun personal-ruby-mode-hook ()
         (inf-ruby-minor-mode +1)
         (ruby-tools-mode +1)
         (subword-mode +1))
       (add-hook 'ruby-mode-hook 'personal-ruby-mode-hook))))

(defvar personal/clojure-prettify-alist '()
  "Pretty symbols for Clojure.")

(defun personal/find-tag-without-ns (next-p)
  "Try to find functions without their namespace."
  (interactive "P")
  (xref-find-definitions
   (car (last (split-string (symbol-name (symbol-at-point)) "/")))))

(use-package scss-mode
  :ensure t
  :defer t
  :config
  (setq scss-compile-at-save nil))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-identifiers-mode)
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.clj(x|s)?$" . clojure-mode))
  (define-clojure-indent
    (defroutes 'defun)
    (s/defn 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (with-mock 2)
    (for-all 2)
    (clone-for 2)
    (alter-when 2)
    (keep-when 2)
    (remove-when 2))

  (eval-after-load 'clojure-mode
    '(setq clojure--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   clojure--prettify-symbols-alist)))
  (eval-after-load 'lisp-mode
    '(setq lisp--prettify-symbols-alist
           (append personal/clojure-prettify-alist
                   lisp--prettify-symbols-alist)))
  :bind
  (("M-." . personal/find-tag-without-ns)))

(use-package cl-lib
  :ensure t)

(use-package clj-refactor
  :ensure t
  :config
  (add-hook
   'clojure-mode-hook
   (lambda ()
     (clj-refactor-mode 1)
     (cljr-add-keybindings-with-prefix "C-c C-a"))))

(use-package cljr-helm
  :ensure t)

(use-package clojure-snippets
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on))

(use-package datomic-snippets
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :config
  (setq cider-prefer-local-resources t
        cider-repl-display-help-banner nil
        cider-repl-history-file (expand-file-name "cider-repl.history" user-emacs-directory)
        cider-repl-history-size 1000
        cider-repl-use-pretty-printing t
        cider-prompt-for-symbol nil
        cider-repl-wrap-history t
        nrepl-hide-special-buffers t
        nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  (advice-add 'cider-find-var :after #'recenter-top-bottom)
  (defadvice cider-pprint-eval-last-sexp
    (after cider-pprint-eval-last-sexp)
    (other-window 1))
  (ad-activate 'cider-pprint-eval-last-sexp))

(use-package sayid
  :ensure t
  :defer t
  :config
  (eval-after-load 'clojure-mode
    '(sayid-setup-package))

  (defadvice sayid-get-workspace
      (after sayid-get-workspace activate)
    (other-window 1))

  (ad-activate 'sayid-get-workspace)

  (defadvice sayid-show-traced
      (after sayid-show-traced activate)
    (other-window 1))

  (ad-activate 'sayid-show-traced))

;; https://curiousprogrammer.wordpress.com/2009/02/11/simple-emacs-shortcut/
(defun duplicate-current-line ()
  "Duplicate the current line."
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

;;; --- misc keybindings ---

(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-o") 'other-frame)
(global-set-key (kbd "s-l") 'avy-goto-line)

(global-set-key (kbd "C-c d") 'duplicate-current-line)

(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x r I") 'string-insert-rectangle)

(global-set-key (kbd "C-x P") 'print-buffer)
(global-set-key (kbd "M-p") 'fill-paragraph)

(global-set-key (kbd "C-c M-t") 'transpose-sentences)
(global-set-key (kbd "C-x M-t") 'transpose-paragraphs)

;;; --- org mode ---

(use-package org
  :ensure t
  :config
  (require 'ob-clojure)
  (require 'cider)
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-log-done 'note
        org-return-follows-link t
        org-startup-indented t
        org-html-validation-link nil
        org-export-html-postamble nil
        org-export-backends '(ascii html icalendar latex md)
        org-use-sub-superscripts "{}"
        org-agenda-show-log t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-include-diary t
        org-agenda-files '("~/Dropbox/org/agenda.org"
                           "~/Dropbox/org/notes.org"
                           "~/Dropbox/org/pointslope/business.org"))
  (setq org-todo-keywords '((type "TODO" "STARTED" "FINISHED" "DELIVERED" "|" "DONE"))
        org-todo-keyword-faces
        '(("TODO"      . (:background "salmon" :foreground "red" :weight bold))
          ("STARTED"   . (:background "light yellow" :foreground "brown" :weight bold))
          ("FINISHED"  . (:background "light blue" :foreground "dark blue" :weight bold))
          ("DELIVERED" . (:background "orange" :foreground "black" :weight bold))
          ("DONE"      . (:background "light green" :foreground "dark green" :weight bold))))

  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* TODO %^{Task} %^G")

          ("b" "Business" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nSCHEDULED: %^t\n")

          ("d" "Deadline" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nDEADLINE: %^t\n")

          ("e" "Emergency" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* STARTED %^{Task}" :clock-in :clock-resume)))

  (setq org-tag-alist '(("business" . "?b")
                        ("personal" . "?p")
                        ("pointslope" . "?o")
                        ("kip" . "?k")
                        ("sales" . "?s")
                        ("tech" . "?t")
                        ("education" . "?e")
                        ("basketball" . "?b")
                        ("fishing" . "?f")
                        ("buppy" . "?y")
                        ("randi" . "?r")
                        ("nikki" . "?n")
                        ("mom" . "?m")
                        ("dad" . "?d")
                        ("wes" . "?w")))

  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode)
              (smartparens-mode -1)))

  (setq org-babel-clojure-backend 'cider
        org-fontify-done-headline t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh         . t)
     (clojure    . t)
     (java       . t)
     (js         . t)
     (ruby       . t)
     (python     . t)))

  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))
  (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t)
  (setq org-bullets-bullet-list
        '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦"
          "◯" "⚪" "⚫" "⚬" "￮" "⊙" "⊚" "∙" "∘")))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.4.1/"))

(use-package org-beautify-theme
  :ensure t)

(use-package htmlize
  :ensure t)

;;; --- custom ---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(cider-inject-dependencies-at-jack-in nil)
 '(cider-pprint-fn (quote puget))
 '(cider-prefer-local-resources t)
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-history-file "/Users/cr/.emacs.d/cider-repl.history")
 '(cider-repl-history-size 1000)
 '(cider-repl-use-pretty-printing t)
 '(cider-repl-wrap-history t)
 '(custom-safe-themes
   (quote
    ("95f80c9b1ae8afcc2c8d66750252b4d6ae19aef46c2d458c5fe5911e6f09d0ce" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "19f68ed86c05e6810925c2985f873f7ad728079ade01f5844d7d61e82dcbae4a" "8dc4a35c94398efd7efee3da06a82569f660af8790285cd211be006324a4c19a" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" default)))
 '(cycle-themes-mode t)
 '(desktop-save-mode nil)
 '(direnv-always-show-summary t)
 '(direnv-mode t)
 '(direnv-show-paths-in-summary nil)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case --ignore=.git")
 '(helm-ag-fuzzy-match t)
 '(helm-ag-use-agignore t)
 '(helm-follow-mode-persistent t)
 '(package-selected-packages
   (quote
    (inf-ruby darkokai-theme darktooth-theme noctilux-theme smyx-theme helm-themes ujelly-theme dracula-theme spaceline-all-the-icons spacemacs-theme spaceline all-the-icons-dired solaire-mode dockerfile-mode cider-eval-sexp-fu mips-mode rainbow-identifiers php-mode org-tree-slide org-slide-tree org-beautify-theme zpresent epresent company-go go-eldoc go-mode helm-ag bookmark+ kibit-mode ox-reveal org flyspell-correct-helm flyspell-mode easy-mark yari ruby-tools scss-mode ov gist 4clojure alchemist elixir-mode web-mode moe-theme base16-theme alect-themes use-package)))
 '(safe-local-variable-values
   (quote
    ((setq cider-boot-parameters "dev")
     (cider-inject-dependencies-at-jack-in . t)
     (cider-cljs-lein-repl . "(do (require 'figwheel-sidecar.repl-api)
                                      (figwheel-sidecar.repl-api/start-figwheel!)
                                      (figwheel-sidecar.repl-api/cljs-repl))")
     (cider-inject-dependencies-at-jack-in)
     (projectile-project-type . lein-test)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; (provide '.emacs)
;;; .emacs ends here
