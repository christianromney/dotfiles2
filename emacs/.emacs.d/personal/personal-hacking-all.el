;; Other Languages
(require 'prelude-go)
(require 'prelude-haskell)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-yaml)

;; Projectile caching
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t))

(use-package projectile-direnv
  :ensure t
  :config
  (add-hook 'projectile-mode-hook 'projectile-direnv-export-variables))

;; TAGS management
(use-package ctags
  :ensure t
  :config
  (setq path-to-ctags "/usr/local/bin/ctags"
        projectile-tags-command "/usr/local/bin/ctags -Re %s %s"
        tags-revert-without-query t)
  :bind
  (("M-." . ctags-search)))

;; DevOps, too

(use-package terraform-mode
  :ensure t
  :defer t)

;; (use-package yasnippet
;;   :ensure t
;;   :diminish (yas-minor-mode . " Ŷ")
;;   :config
;;   (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))
;;   (defun do-yas-expand ()
;;     (let ((yas-fallback-behavior 'return-nil))
;;       (yas-expand)))
;;   ;; bind to TAB in keybindings file
;;   (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas-minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common)
;;             (indent-for-tab-command)))))
;;   (add-to-list 'yas-snippet-dirs
;;                (expand-file-name "snippets" prelude-personal-dir))
;;   (define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)
;;   (define-key yas-minor-mode-map (kbd "<tab>") 'tab-indent-or-complete))
;;
;;
;; (use-package ctags-update
;;  :ensure t
;;  :diminish ctags-auto-update-mode
;;  :commands ctags-update
;;  :config
;;  (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
;;  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))
