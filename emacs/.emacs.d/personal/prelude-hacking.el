(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . " Ŷ")
  :config
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))
  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))
  ;; bind to TAB in keybindings file
  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" prelude-personal-dir))
  (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
  (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete))

(use-package projectile
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode t)))

(use-package js2-mode
  :ensure t
  :config
  (setq auto-mode-alist (cons '("\\.template$" . js2-mode) auto-mode-alist))
  (add-hook 'js2-mode-hook 'yas-minor-mode-on))

;; Zen Coding
(use-package web-mode
  :ensure t
  :config
  (progn
    (defun personal/disable-smartparens ()
      (smartparens-mode 0))

    (defun personal/sp-web-mode-is-code-context (id action context)
      (when (and (eq action 'insert)
                 (not (or (get-text-property (point) 'part-side)
                          (get-text-property (point) 'block-side))))
        t))
    (defun personal/web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-enable-auto-pairing t)
      (add-to-list 'sp-ignore-modes-list 'web-mode)))
  (setq auto-mode-alist (cons '("\\.php$" . web-mode) auto-mode-alist))
  (add-hook 'web-mode-hook  'personal/web-mode-hook)
  (add-hook 'web-mode-hook 'personal/disable-smartparens))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'emmet-mode-hook 'yas-minor-mode-on))

;; TAGS management

(use-package ctags
  :ensure t
  :config
  (setq path-to-ctags "/usr/local/bin/ctags"
        projectile-tags-command "/usr/local/bin/ctags -Re %s %s"
        tags-revert-without-query t)
  :bind
  (("M-." . ctags-search)))

(use-package ctags-update
 :ensure t
 :diminish ctags-auto-update-mode
 :commands ctags-update
 :config
 (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
 (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))
