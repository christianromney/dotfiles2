;;; Window Movement
(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

;;; Commenting and Killing
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Utilities
(global-set-key (kbd "C-c =") 'prelude-increment-integer-at-point)
(global-set-key (kbd "C-c _") 'prelude-decrement-integer-at-point)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Buffer shortcuts
(global-set-key (kbd "C-x p") 'print-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; Otherwise Helm is unusable
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)

(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; Misc
(global-set-key (kbd "M-p") 'fill-paragraph)

(define-key prelude-mode-map (kbd "M-o") 'other-window)
(define-key prelude-mode-map (kbd "s-o") 'other-frame)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-o") 'other-frame)

(global-set-key (kbd "<f8>") 'pivotal)

;; aligning
(define-key clojure-mode-map (kbd "C-x a c") 'align-current)
(define-key clojure-mode-map (kbd "C-x a j") 'align-cljlet)

(defun personal-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(defun personal-just-one-space ()
  (interactive)
  (just-one-space -1))

(defun personal-sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(define-key smartparens-mode-map
  (kbd "M-\\") 'personal-delete-horizontal-space)

(define-key smartparens-mode-map
  (kbd "M-SPC") 'personal-just-one-space)

;; mnemonic 'l' for lines
;; mnemonic 'a' for beginning (emacs style)
;; mnemonic 'e' for end (emacs style)
;; mnemonic 'h' for html
;; mnemonic 't' for tag
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m l" . mc/edit-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m h" . mc/mark-all-like-this-dwim)
   ("C-c m t" . mc/mark-sgml-tag-pair)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :diminish (smartparens-mode . " (Sm)"))
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(personal-sp-web-mode-is-code-context))
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
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp))

(provide 'personal/prelude-zbindings)
