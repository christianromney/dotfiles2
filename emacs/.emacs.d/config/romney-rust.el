;;; romney-rust.el --- Rust programming
;;; Commentary:
;;
;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map ("C-c <tab>" . rust-format-buffer))
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :ensure t
  :defer t
  :hook (rust-mode . racer-mode)
  :bind (:map racer-mode-map
              ("C-c m h" . racer-describe)
              ("C-c m d" . racer-debug))
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package company-racer
  :ensure t
  :defer t
  :after racer
  :config
  (add-to-list 'company-backends 'company-racer))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode)
  :bind (:map rust-mode-map
              ("<f6>" . cargo-process-build))
  :config
  (setq compilation-scroll-output t)
  (defadvice cargo-process-build
      (after cargo-process-build activate)
    (other-window 1)))

(use-package toml-mode
  :ensure t
  :defer t)

(romney/focus-command-windows
 '(cargo-process-run
   cargo-process-fmt
   cargo-process-check
   cargo-process-clean
   cargo-process-search
   cargo-process-test
   cargo-process-doc))

(provide 'romney-rust)
;;; romney-rust.el ends here
