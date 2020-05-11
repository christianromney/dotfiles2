;;; ~/.doom.d/behavior.el -*- lexical-binding: t; -*-
;;
;; Always center when jumping to search results
(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

(defadvice isearch-forward
    (after isearch-forward-recenter activate)
  (recenter))

(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (recenter))

(ad-activate 'isearch-repeat-forward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (recenter))

(ad-activate 'isearch-repeat-backward)
