;;; personal/prelude-zbindings --- Keybindings the way I like them
;;;
;;; Commentary:
;;;
;;; Some package-specific keybindings are grouped under their
;;; use-package configuration in other configuration files
;;;
;;; Code:
;;;
;;; iTerm2 / Emacs compatibility

(global-set-key "\e[OA" [M-up])
(global-set-key "\e[OB" [M-down])
(global-set-key "\e[OC" [M-right])
(global-set-key "\e[OD" [M-left])

(global-set-key "\e[1;9A" [M-S-up])
(global-set-key "\e[1;9B" [M-S-down])
(global-set-key "\e[1;9C" [M-S-right])
(global-set-key "\e[1;9D" [M-S-left])

;;; Window Movement
(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

;;; Commenting and Killing
(global-set-key (kbd "C-c M-/") 'comment-region)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Utilities
(global-set-key (kbd "C-c =") 'prelude-increment-integer-at-point)
(global-set-key (kbd "C-c _") 'prelude-decrement-integer-at-point)

;; Replace prelude visit term with multi-term
(define-key prelude-mode-map (kbd "C-c t") 'multi-term)
(define-key prelude-mode-map (kbd "C-c o") nil)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Buffer shortcuts
(global-set-key (kbd "C-x p") 'print-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(use-package multiple-cursors
  :bind
  (("C-c m l" . mc/edit-lines)                  ;; mnemonic 'l' for lines
   ("C-c m a" . mc/edit-beginnings-of-lines)    ;; mnemonic 'a' for beginning (emacs style)
   ("C-c m e" . mc/edit-ends-of-lines)          ;; mnemonic 'e' for end (emacs style)
   ("C-c m h" . mc/mark-all-like-this-dwim)     ;; mnemonic 'h' for html
   ("C-c m t" . mc/mark-sgml-tag-pair)          ;; mnemonic 't' for tag
   ))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Fix Helm to work the way I want
(use-package helm-mode
  :config
  (progn
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)))

;; Window Management
(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)
(global-set-key (kbd "C-c w p") 'buf-move-up)
(global-set-key (kbd "C-c w n") 'buf-move-down)
(global-set-key (kbd "C-c w b") 'buf-move-left)
(global-set-key (kbd "C-c w f") 'buf-move-right)
(global-set-key (kbd "C-c w .") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w ,") 'enlarge-window-horizontally)

(global-set-key (kbd "M-y") 'browse-kill-ring)
(global-set-key (kbd "M-p") 'fill-paragraph)


(use-package smartparens
  :init
  (progn
    (defun personal/delete-horizontal-space ()
      (interactive)
      (just-one-space -1)
      (sp-backward-delete-char))

    (defun personal/just-one-space ()
      (interactive)
      (just-one-space -1))
    
    (diminish 'smartparens-mode " (Sm)"))

  :bind
  (("M-\\" . personal/delete-horizontal-space)
   ("M-SPC" . personal/just-one-space))
  
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(personal/sp-web-mode-is-code-context))

    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))
    
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
    (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)
    
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)))

(provide 'personal/prelude-zbindings)
;;; prelude-zbindings.el ends here
