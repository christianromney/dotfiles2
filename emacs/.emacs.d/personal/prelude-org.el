(require 'deft)

;; Export
(setq org-html-validation-link nil)
(setq org-export-html-postamble nil)
(setq org-export-backends '(ascii html icalendar latex md))

;; File handling
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/default.org"))
(setq org-log-done t)
(setq deft-directory "~/Dropbox/org")
(setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(global-set-key [f8] 'deft)

;; TODOs
(setq org-todo-keywords '((type "TODO" "IN-PROGRESS" "|" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))

;;
(require 'org-trello)
(custom-set-variables '(org-trello-files '("~/Dropbox/org/trello-scorbot.org")))
(add-hook 'org-mode-hook 'org-trello-mode)

;; Agenda
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t
      org-agenda-include-diary t)

(setq org-agenda-files (list "~/Dropbox/org/agenda.org"))

;; Run these, too
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

;; Babel

(require 'ob)
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(setq org-fontify-done-headline t)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-src-window-setup 'current-window)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh         . t)
   (clojure    . t)
   (java       . t)
   (js         . t)
   (ruby       . t)
   (python     . t)))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
(add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))
