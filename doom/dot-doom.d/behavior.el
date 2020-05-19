;;; ~/.doom.d/behavior.el -*- lexical-binding: t; -*-
(defun private/after-jump ()
  (recenter-top-bottom)
  (+nav-flash/blink-cursor))

;; Always center when jumping to search results
(add-hook 'isearch-mode-end-hook 'private/after-jump)
(add-hook 'imenu-after-jump-hook 'private/after-jump)

(defadvice isearch-forward
    (after isearch-forward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-repeat-forward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-repeat-backward)
