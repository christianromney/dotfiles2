;; General programming and language settings not deserving of their own files
(prelude-require-package 'yasnippet)
(yas-global-mode +1)
(add-to-list 'yas-snippet-dirs
      (expand-file-name "snippets" prelude-personal-dir))

;; Complete everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; fix company / yas / indent bullshit
;; thx http://www.emacswiki.org/emacs/CompanyMode#toc9
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

;; bind to TAB in keybindings file 
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(setq vc-follow-symlinks t)

;; Git gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines t)

;; Ctags
(setq tags-revert-without-query t)
(global-set-key (kbd "<f7>") 'ctags-create-or-update-tags-table)
(global-set-key (kbd "M-.")  'ctags-search)

;; Merge
(setq emerge-diff-options "--ignore-all-space")

;; Project management w/ projectile
(projectile-global-mode t)
(setq projectile-enable-caching t)

;; AWS
(setq auto-mode-alist (cons '("\\.template$" . js-mode) auto-mode-alist))

;; Zen Coding
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'web-mode-hook)

;; Adapted from Emacs Live
(defun romney-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(defun romney-just-one-space ()
  (interactive)
  (just-one-space -1))

;; TAGS management
(autoload 'turn-on-ctags-auto-update-mode
  "ctags-update" "turn on ctags-auto-update-mode." t)

(add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)

(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
