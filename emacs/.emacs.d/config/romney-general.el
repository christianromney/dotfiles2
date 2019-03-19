;;; romney-general.el --- General purpose packages
;;; Commentary:
;;
;;; Code:
(use-package tramp
  :ensure nil
  :defer t
  :defines (tramp-default-method)
  :config (setq tramp-default-method "ssh"))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package typo  ;; Automatically use typographic quotes
  :ensure t
  :hook ((org-mode      . typo-mode)
         (markdown-mode . typo-mode)
         (rst-mode      . typo-mode))
  :init (setq-default typo-language "English"))

(use-package writeroom-mode
  :ensure t
  :defer t
  :bind (("<f7>" . writeroom-mode)
         :map writeroom-mode-map
         ("C-M-<" . writeroom-decrease-width)
         ("C-M->" . writeroom-increase-width)
         ("C-M-=" . writeroom-adjust-width))
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package ggtags
  :ensure t
  :pin melpa-stable
  :defer t)

(use-package abbrev
  :diminish abbrev-mode
  :ensure nil
  :defer t
  :config
  (add-hook 'text-mode-hook #'abbrev-mode)
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package paren ;; show matching parens
  :ensure nil
  :config
  (show-paren-mode +1))

(use-package saveplace ;; remember location when saving files
  :config
  (setq save-place-file (expand-file-name "saved-places" personal-data-dir))
  (save-place-mode))

(use-package savehist ;; autosave work
  :defer 5
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "autosave-history" personal-data-dir))
  (savehist-mode +1))

(use-package recentf
  :defer 3
  :config
  (setq recentf-save-file (expand-file-name "recent-files" personal-data-dir)
        recentf-max-saved-items 100
        recentf-max-menu-items  20
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove ;; use shift + arrow keys to nav windows
  :defer 3
  :config
  (windmove-default-keybindings))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package anzu ;; search & replace match info e.g. 1 of N
  :ensure t
  :defer t
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill ;; copy with shortcuts
  :ensure t
  :defer t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell ;; find programs on shell $PATH
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package move-text ;; move text blocks around
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package alert
  :ensure t
  :config
  (setq-default alert-default-style 'osx-notifier))

(use-package smex ;; smarter M-x : is this needed with counsel???
  :ensure t
  :defer t
  :config
  (setq smex-save-file (expand-file-name "smex-items" personal-data-dir))
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package company ;; COMPlete ANYthing
  :ensure t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 20
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-flip-when-above t)
  (global-company-mode 1))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(use-package pos-tip
  :ensure t
  :defer t)

(use-package super-save ;; save buffers on lost focus
  :ensure t
  :defer 5
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
   ("C-c d"                        . crux-duplicate-current-line-or-region)
   ("M-O"                          . crux-smart-open-line)
   ("C-c u"                        . crux-view-url)
   ("C-c e"                        . crux-eval-and-replace)
   ("C-c w"                        . crux-swap-windows)
   ("C-c D"                        . crux-delete-file-and-buffer)
   ("C-c K"                        . crux-kill-other-buffers)
   ("C-c I"                        . crux-find-user-init-file)
   ("C-c R"                        . crux-rename-buffer-and-file)
   ("C-c TAB"                      . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c s"                        . crux-ispell-word-then-abbrev)
   ("s-j"                          . crux-top-join-line)
   ("s-k"                          . crux-kill-whole-line)
   ("s-o"                          . crux-smart-open-line-above)
   ("C-<backspace>"                . crux-kill-line-backwards)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([(shift return)]               . crux-smart-open-line)
   ([(control shift return)]       . crux-smart-open-line-above)
   ([remap kill-whole-line]        . crux-kill-whole-line)))

(use-package undo-tree ;; better undo / redo
  :ensure t
  :defer t
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package which-key ;; help remember keybindings
  :ensure t
  :config
  (which-key-mode +1))

(use-package flyspell
  :ensure t
  :defer t
  :bind
  (("C-;" . flyspell-correct-previous)))

(use-package define-word
  :ensure t
  :defer t
  :bind ("C-c ?" . define-word-at-point))

(use-package epg
  :ensure t
  :config
  (setenv "GPG_AGENT_INFO" nil))

;; string handling routines
(use-package s :ensure t)
(use-package eww
  :defer t
  :init
  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  :bind
  (:map eww-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   :map eww-link-keymap
   ("o" . eww)
   ("O" . eww-browse-with-external-browser)))

;; --- functions ---
(defun romney/dired-config ()
  "Open the config directory in dired."
  (interactive)
  (dired personal-config-dir))

(defun romney/focus-other-window (&rest opts)
  "Focus the other window.
OPTS - ignored. varargs used here to allow calling from a multiple of contexts."
  (other-window 1))

(defun romney/focus-command-windows (commands)
  "Focus the other window when any of the given commands are executed.
COMMANDS - a list of symbols to advise with romney/focus-other-window."
  (let (_value)
    (dolist (command commands _value)
      (advice-add command :after #'romney/focus-other-window))))

(romney/focus-command-windows
 '(describe-key
   describe-function
   describe-package
   describe-variable
   describe-mode
   describe-bindings))

(provide 'romney-general)
;;; romney-general.el ends here
