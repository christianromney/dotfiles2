;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-
;;
;; Global
(map! "C-x \\"  #'align-regexp
      "C-x g"   #'magit-status
      "M-/"     #'hippie-expand
      "C-x \\"  #'align-regexp
      "M-o"     #'other-window
      "C-'"     #'avy-goto-line
      "C-:"     #'avy-goto-char
      "C-C M-o" #'occur
      "C-x r I" #'string-insert-rectangle
      "C-x P"   #'print-buffer
      "M-p"     #'fill-paragraph
      "C-x C-h" #'add-file-local-variable-prop-line
      "C-x M-s" #'transpose-sexps
      "C-c M-t" #'transpose-sentences
      "C-x M-t" #'transpose-paragraphs
      "C-c a"   #'org-agenda
      "M-%"     #'anzu-query-replace
      "C-M-%"   #'anzu-query-replace-regexp
      "<f5>"    #'deadgrep)

;; Clojure mode
(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs)

;; Ivy navigation
(map! :map ivy-minibuffer-map
      "C-m"     #'ivy-alt-done
      "C-l"     #'counsel-up-directory)

;; Dired navigation
(map! :map dired-mode-map
      "C-l"     #'dired-up-directory)
