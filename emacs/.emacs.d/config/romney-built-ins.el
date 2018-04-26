;;; romney-built-ins.el --- Configuration for packages that are built into Emacs

;;; Commentary:
;;
;;; Code:
(require 'subword)
(global-subword-mode +1)
(define-key subword-mode-map (vector 'remap 'transpose-words) nil)
(define-key subword-mode-map (vector 'remap 'upcase-word) nil)
(define-key subword-mode-map (vector 'remap 'downcase-word) nil)

(require 'misc)
(global-set-key (kbd "M-Z")     'zap-up-to-char)
(global-set-key (kbd "C-x f")   'find-file)         ;; typo elimination
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x \\")  #'align-regexp)

(require 'which-func)
(which-function-mode 1)                             ;; shows applicable key bindings

(require 'bookmark)
(setq bookmark-save-flag 1
      bookmark-default-file
      (expand-file-name "bookmarks" personal-savefile-dir))

(require 'midnight)                                 ;; clean up obsolete buffers automatically

;;; --- saner regexp syntax ---
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'compile)
(setq compilation-ask-about-save nil                ;; Just save before compiling
      compilation-always-kill t                     ;; Just kill old compile processes before
      compilation-scroll-output 'first-error)       ;; Automatically scroll to first error

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t                ;; rename after killing uniquified
      uniquify-ignore-buffers-re "^\\*"             ;; don't muck with special buffers
      )

(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-dwim-target t)

;;; --- enable various 'off by default' features ---

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)

;;; --- search behavior ---

(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)
(defadvice
    isearch-forward
    (after isearch-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-forward)

(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-backward)

(provide 'romney-built-ins)
;;; romney-built-ins.el ends here
