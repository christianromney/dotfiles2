;; This should be obvious...
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Export
(setq org-html-validation-link nil)
(setq org-export-html-postamble nil)

;; File handling
;; (setq org-directory "~/Dropbox/Notes")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq org-log-done 'time)

;; TODO
(setq org-todo-keywords
      '((type "NEW" "IN-PROGRESS" "|" "DONE")))



(setq org-export-backends '(ascii html icalendar latex md))

;; Babel

;; (setq org-fontify-done-headline t)
;; (setq org-src-fontify-natively t)
;; (setq org-confirm-babel-evaluate nil)
;; (setq org-src-window-setup 'current-window)


;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (sh         . t)
;;    (clojure    . t)
;;    (java       . t)
;;    (js         . t)
;;    (ruby       . t)
;;    (python     . t)
;;    (R          . t)))

;; (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
;; (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))
