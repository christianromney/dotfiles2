;;q Fix keybindings under iTerm2
(global-set-key "\e[1;9A" [M-up])
(global-set-key "\e[1;9B" [M-down])
(global-set-key "\e[1;9C" [M-right])
(global-set-key "\e[1;9D" [M-left])

(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

(global-set-key (kbd "C-c M-/") 'comment-region)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)

;; Utilities
(global-set-key (kbd "C-c =") 'prelude-increment-integer-at-point)
(global-set-key (kbd "C-c _") 'prelude-decrement-integer-at-point)

;; (global-unset-key (kbd "C-z"))

(require 'ace-jump-buffer)
(global-set-key (kbd "C-c C-o") 'ace-jump-buffer)

;; Ctrl-x r i Useful rectangle binding
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; Buffer shortcuts
(global-set-key (kbd "C-x p") 'print-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Ctrl-= Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Ctrl-C m c - Multiple Cursors
(global-set-key (kbd "C-c m l") 'mc/edit-lines)                  ;; mnemonic 'l' for lines
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)    ;; mnemonic 'a' for beginning (emacs style)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)          ;; mnemonic 'e' for end (emacs style)
(global-set-key (kbd "C-c m h") 'mc/mark-all-like-this-dwim)     ;; mnemonic 'h' for html
(global-set-key (kbd "C-c m t") 'mc/mark-sgml-tag-pair)          ;; mnemonic 't' for tag

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

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)
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

(define-key sp-keymap (kbd "M-\\") 'romney-delete-horizontal-space)
(define-key sp-keymap (kbd "M-SPC") 'romney-just-one-space)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; Could this be the cause of my pain?
;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;; F*ck!
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [tab] 'tab-indent-or-complete)

;; Haskell Mode
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
(define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
(define-key haskell-mode-map (kbd "C-c M-.") nil)
(define-key haskell-mode-map (kbd "C-c C-d") nil)
(define-key haskell-mode-map (kbd "C-x C-d") nil)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

;; Cabal Mode
;; (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
