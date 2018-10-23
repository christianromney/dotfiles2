;;; romney-keybindings.el --- Global keybindings
;;; Commentary:
;;
;;; Code:
;;; --- global keybindings ---

(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-=") 'text-scale-increase)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-o") 'other-frame)
(global-set-key (kbd "s-l") 'avy-goto-line)
(global-set-key (kbd "C-:") 'avy-goto-char)

(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x r I") 'string-insert-rectangle)

(global-set-key (kbd "C-x P") 'print-buffer)
(global-set-key (kbd "M-p") 'fill-paragraph)

(global-set-key (kbd "C-c M-t") 'transpose-sentences)
(global-set-key (kbd "C-x M-t") 'transpose-paragraphs)

;; terminal decoding
;; make sure iTerm2 is sending these escape sequences
(define-key input-decode-map "\e[A" [up])
(define-key input-decode-map "\e[B" [down])
(define-key input-decode-map "\e[C" [right])
(define-key input-decode-map "\e[D" [left])

(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(define-key input-decode-map "\e[1;2C" [S-right])
(define-key input-decode-map "\e[1;2D" [S-left])

(define-key input-decode-map "\e[1;3A" [M-up])
(define-key input-decode-map "\e[1;3B" [M-down])
(define-key input-decode-map "\e[1;3C" [M-right])
(define-key input-decode-map "\e[1;3D" [M-left])

(define-key input-decode-map "\e[1;4A" [M-S-up])
(define-key input-decode-map "\e[1;4B" [M-S-down])
(define-key input-decode-map "\e[1;4C" [M-S-right])
(define-key input-decode-map "\e[1;4D" [M-S-left])

(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\e[1;5C" [C-right])
(define-key input-decode-map "\e[1;5D" [C-left])

(define-key input-decode-map "\e[1;6A" [C-S-up])
(define-key input-decode-map "\e[1;6B" [C-S-down])
(define-key input-decode-map "\e[1;6C" [C-S-right])
(define-key input-decode-map "\e[1;6D" [C-S-left])

(define-key input-decode-map "\e[1;7A" [C-M-up])
(define-key input-decode-map "\e[1;7B" [C-M-down])
(define-key input-decode-map "\e[1;7C" [C-M-right])
(define-key input-decode-map "\e[1;7D" [C-M-left])

(define-key input-decode-map "\e[1;8A" [C-M-S-up])
(define-key input-decode-map "\e[1;8B" [C-M-S-down])
(define-key input-decode-map "\e[1;8C" [C-M-S-right])
(define-key input-decode-map "\e[1;8D" [C-M-S-left])

(define-key input-decode-map "\e[1;9A" [s-up])
(define-key input-decode-map "\e[1;9B" [s-down])
(define-key input-decode-map "\e[1;9C" [s-right])
(define-key input-decode-map "\e[1;9D" [s-left])

(define-key input-decode-map "\e[1;10A" [s-S-up])
(define-key input-decode-map "\e[1;10B" [s-S-down])
(define-key input-decode-map "\e[1;10C" [s-S-right])
(define-key input-decode-map "\e[1;10D" [s-S-left])

(provide 'romney-keybindings)
;;; romney-keybindings.el ends here
