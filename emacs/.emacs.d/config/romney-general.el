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
  :defer 10
  :init
  (require 'saveplace)
  :config
  (setq save-place-file
        (expand-file-name "saveplace" personal-savefile-dir))
  (setq-default save-place t))

(use-package savehist ;; autosave work
  :defer 5
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file
        (expand-file-name "savehist" personal-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :defer 3
  :config
  (setq recentf-save-file
        (expand-file-name "recentf" personal-savefile-dir)
        recentf-max-saved-items 500
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
  (ido-mode +1))

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
  :defer t
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
  :ensure t
  :defer t)

(use-package helm
  :ensure t
  :pin melpa-stable
  :diminish helm-mode
  :defines (helm-split-window-in-side-p
            helm-M-x-fuzzy-match
            helm-buffers-fuzzy-matching
            helm-recentf-fuzzy-match
            helm-semantic-fuzzy-match
            helm-imenu-fuzzy-match
            helm-locate-fuzzy-match
            helm-apropos-fuzzy-match
            helm-lisp-fuzzy-completion
            helm-ff-search-library-in-sexp
            helm-ff-file-name-history-use-recentf
            helm-command-map
            helm-grep-default-command
            helm-grep-default-recurse-command)
  :bind
  (("M-x"      . helm-M-x)
   ("C-x C-m"  . helm-M-x)
   ("C-c h"    . helm-command-prefix)
   ("M-i"      . helm-semantic-or-imenu)
   ("M-y"      . helm-show-kill-ring)
   ("C-x b"    . helm-mini)
   ("C-x C-b"  . helm-buffers-list)
   ("C-x C-f"  . helm-find-files)
   ("C-x r b"  . helm-filtered-bookmarks)
   ("C-h f"    . helm-apropos)
   ("C-h r"    . helm-info-emacs)
   ("C-h C-l"  . helm-locate-library)
   :map helm-map
   ("<tab>"     . helm-execute-persistent-action)
   ("C-i"       . helm-execute-persistent-action)
   ("C-z"       . helm-select-action))
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
        helm-autoresize-mode t
        helm-autoresize-max-height            50
        helm-autoresize-min-height            20
        helm-follow-mode-persistent           t
        helm-google-suggest-use-curl-p        t
        helm-grep-default-command             "ack -Hn -i --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command     "ack -H -i --no-group --no-color %e %p %f")
  (substitute-key-definition 'xref-find-definitions 'helm-etags-select global-map)
  (helm-mode +1)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (when (executable-find "rg")
    (setq helm-grep-ag-command
          "rg --color=always --colors 'match:fg:black' --colors 'match:bg:magenta' --smart-case --no-heading --line-number %s %s %s"
          helm-grep-ag-pipe-cmd-switches
          '("--colors 'match:fg:black'" "--colors 'match:bg:magenta'"))))

(defun romney/focus-other-window (&rest opts)
  "Focus the other window"
  (other-window 1))

(defun romney/focus-command-windows (commands)
  "Focus the other window when any of the given commands are executed"
  (let (_value)
    (dolist (command commands _value)
      (advice-add command :after #'romney/focus-other-window))))

(romney/focus-command-windows
 '(helm-man-woman
   describe-key
   describe-function
   describe-package
   describe-variable
   describe-mode
   describe-bindings))

(use-package helm-core
  :ensure t
  :pin melpa-stable
  :defer t)

(use-package helm-descbinds
  :ensure t
  :pin melpa-stable
  :defer t
  :config
  (helm-descbinds-mode))

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
   ("C-c n"                        . crux-cleanup-buffer-or-region)
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
   ("C-c s"                        . crux-ispell-word-then-abbrev)
   ("s-j"                          . crux-top-join-line)
   ("s-k"                          . crux-kill-whole-line)
   ("C-<backspace>"                . crux-kill-line-backwards)
   ("s-o"                          . crux-smart-open-line-above)
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

(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell helm)
  :config (require 'flyspell-correct-helm))

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

(use-package epg
  :ensure t
  :config
  (setenv "GPG_AGENT_INFO" nil))

(provide 'romney-general)
;;; romney-general.el ends here
