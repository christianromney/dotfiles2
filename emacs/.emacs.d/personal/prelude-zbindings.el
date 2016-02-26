;; Text Scaling
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

;; Utilities
(global-set-key (kbd "C-c =") 'prelude-increment-integer-at-point)
(global-set-key (kbd "C-c -") 'prelude-decrement-integer-at-point)

;; Available!
;; C-c C--
;; C-c C-= 

;;; Window / Frame Movement
(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-o") 'other-frame)
(define-key prelude-mode-map (kbd "M-o") 'other-window)
(define-key prelude-mode-map (kbd "s-o") 'other-frame)

;;; Commenting and Killing
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r I") 'string-insert-rectangle)

;; Buffer shortcuts
(global-set-key (kbd "C-x p") 'print-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; Misc
(global-set-key (kbd "M-p") 'fill-paragraph)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;; Function Key mappings
;; <f1> help
;; <f2> packages
;; <f3> macrorecord start
;; <f4> macrorecord end
;; <f5> pivotal 
;; <f6> hackernews
(global-set-key (kbd "<f7>") 'newsticker-show-news)
;;(global-set-key (kbd "<f8>" 'available))
;;(global-set-key (kbd "<f9>" 'available))
;; <f10> connect to mysql

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
  :diminish (smartparens-mode . " (Sm)")
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(personal-sp-web-mode-is-code-context))
    (sp-with-modes '(html-mode sgml-mode web-mode)
      (sp-local-pair "<" ">"))

    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
    
    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
    
    (define-key smartparens-mode-map (kbd "s-a") 'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "s-e") 'sp-end-of-sexp)
    
    (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
    (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

    (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
    
    (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)

    (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    ;; note the uppercase "D", also bound to M-s
    (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

    ;; this "<delete>" is the one near the "home" button on larger keyboards (104 vs 88)
    (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)

    ;; Mac "<backspace>" is labeled delete
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    ;; end
    
    ;; to work these backwards, prefix with C-- and C-M--
    (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)
    
    (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
    (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)
    
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)))

(provide 'personal/prelude-zbindings)
