;;; romney-general.el --- General purpose packages
;;; Commentary:
;;
;;; Code:
(use-package tramp
  :ensure nil
  :defer t
  :defines (tramp-default-method)
  :config (setq tramp-default-method "ssh"))

;;; --- automatic encryption handling ---
(use-package epg
  :ensure nil
  :defer t
  :defines (epa-file-cache-passphrase-for-symmetric-encryption)
  :init
  (setenv "GPG_AGENT_INFO" nil)
  :config
  (require 'epa-file)
  (require 'password-cache)
  (setq epg-gpg-program "gpg2"
        password-cache-expiry (* 4 60 60) ;; cache for 4 hours (defaults to 16 seconds)
        epa-pinentry-mode 'loopback
        epa-file-select-keys nil
        epa-file-cache-passphrase-for-symmetric-encryption t)
  (epa-file-enable))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package typo  ;; Automatically use typographic quotes
  :ensure t
  :hook ((org-mode      . typo-mode)
         (markdown-mode . typo-mode))
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

(defun romney/save-place-reposition ()
  "Force windows to recenter current line (with saved position)."
  (run-with-timer 0 nil
                  (lambda (buf)
                    (when (buffer-live-p buf)
                      (dolist (win (get-buffer-window-list buf nil t))
                        (with-selected-window win (recenter)))))
                  (current-buffer)))

(use-package saveplace ;; remember location when saving files
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saved-places" personal-data-dir))
  (save-place-mode)
  (add-hook 'find-file-hook 'romney/save-place-reposition t))

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
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window)))

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
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package move-text ;; move text blocks around
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package smex ;; smarter M-x
  :ensure t
  :defer t
  :config
  (setq smex-save-file (expand-file-name "smex-items" personal-data-dir))
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package company
  :ensure t
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 20            ; bigger popup window
        company-idle-delay 0.2              ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-show-numbers t
        company-dabbrev-downcase nil)

  ;; Icons and quickhelp
  (use-package company-box
    :ensure t
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil
          company-box-show-single-candidate t
          company-box-max-candidates 50)

    (defun company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))

    (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15))
              (Text . ,(all-the-icons-material "text_fields" :height 0.9 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-material "straighten" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blue))
              (Variable . ,(all-the-icons-material "straighten" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blue))
              (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blue))
              (Property . ,(all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blue))
              (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.15))
              (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.15))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.05))
              (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.15)))))))

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

(use-package hydra
  :ensure t
  :defer t)

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
