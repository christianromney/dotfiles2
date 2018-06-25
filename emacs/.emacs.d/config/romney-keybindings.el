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

(global-set-key (kbd "C-c d") 'duplicate-current-line)

(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "C-x r I") 'string-insert-rectangle)

(global-set-key (kbd "C-x P") 'print-buffer)
(global-set-key (kbd "M-p") 'fill-paragraph)

(global-set-key (kbd "C-c M-t") 'transpose-sentences)
(global-set-key (kbd "C-x M-t") 'transpose-paragraphs)

(provide 'romney-keybindings)
;;; romney-keybindings.el ends here
