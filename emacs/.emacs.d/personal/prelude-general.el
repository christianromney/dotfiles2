;;; UTF-8 everywhere
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
      make-backup-files nil)

(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)

(cua-mode 1)

(defun live-copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun live-paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq cua-enable-cua-keys nil
      default-input-method "MacOSX"
      system-name (car (split-string system-name "\\.")))

(when (not window-system)
  (setq interprogram-cut-function 'live-paste-to-osx)
  (setq interprogram-paste-function 'live-copy-from-osx))

(use-package synosaurus
  :ensure t
  :bind
  ("C-x t" . synosaurus-choose-and-replace))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind
  ("C-x C-r" . recentf-open-files))


(use-package helm-mode
  :config
  (helm-autoresize-mode t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-M-x-fuzzy-match t
        helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f")
  :bind
  ("M-y" . helm-show-kill-ring)
  ("M-i" . helm-imenu))

(use-package paradox
  :ensure t
  :bind
  ("<f5>" . paradox-list-packages))

;; Spelling
(use-package flyspell-mode
  :diminish (flyspell-mode . " FSp")
  :config
  (progn
    (setq flyspell-issue-welcome-flag nil)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
    (setq-default ispell-list-command "list")))

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

(use-package adoc-mode
  :ensure t
  :defer t
  :config
  (setq auto-mode-alist (cons '("\\.adoc$"  . adoc-mode) auto-mode-alist)))

(use-package hackernews
  :ensure t
  :defer t
  :bind (("C-c h n" . hackernews)))