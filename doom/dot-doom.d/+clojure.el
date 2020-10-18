;;; ../src/open/dotfiles/doom/dot-doom.d/+clojure.el -*- lexical-binding: t; -*-

;; ===============================================================================
;;                              CLOJURE / CIDER
;; ===============================================================================

;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs
      :map cider-mode-map
      "C-s-x"   #'rebl-eval-defun-at-point
      "C-x C-r" #'rebl-eval-last-sexp)

(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)

(message "Loaded +clojure configuration")
