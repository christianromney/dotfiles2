;;; personal/prelude-org --- Org mode Configuration
;;; Commentary:
;;; Org-mode settings for export, agenda/calendar, todo lists
;;; and blogging to Wordpress
;;; 
;;; Code:
(use-package org
  :init
  ;; Export
  (setq org-html-validation-link nil)
  (setq org-export-html-postamble nil)
  (setq org-export-backends '(ascii html icalendar latex md))

  ;; File handling
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/default.org"))
  (setq org-log-done t
        org-startup-indented t)

  ;; Agenda  
  (setq org-agenda-show-log t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-include-diary t
        org-agenda-files (list "~/Dropbox/org/agenda.org"))

  ;; Todo Lists
  (setq org-todo-keywords '((type "TODO" "IN-PROGRESS" "|" "DONE"))
        org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (writegood-mode)
              (auto-fill-mode))))

(use-package deft
  :init
  (setq deft-directory "~/Dropbox/org")
  (setq deft-use-filename-as-title t)
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode)
  :bind ("<f8>" . deft))

;; Babel
(use-package ob
  :config
  (require 'ob-clojure)
  (require 'cider)
  
  (setq org-babel-clojure-backend 'cider
        org-fontify-done-headline t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window)

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
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js")))


;; Credentials are encrypted in ~/.authinfo.gpg
;; and Emacs can *just read them* if GPG agent is running
(use-package org2blog-autoloads
  :init
  (require 'netrc)
  :config

  (setq org2blog/wp-use-sourcecode-shortcode 't
        org2blog/wp-sourcecode-default-params nil
        org2blog/wp-sourcecode-langs
        '("bash" "clojure" "css"
          "erlang" "fsharp" "diff" "groovy" "javascript" "java"
          "objc" "php" "text" "python" "ruby" "sql"
          "xml" "sh" "emacs-lisp" "lisp"))

  (let* ((creds (netrc-parse "~/.authinfo.gpg"))
         (blog (netrc-machine creds "blog.pointslope.com")))

    (setq org2blog/wp-blog-alist
          '(("pointslope"
             :url "https://www.pointslope.com/blog/xmlrpc.php"
             :username (netrc-get blog "login")
             :password (netrc-get blog "password")
             :default-title "Draft"
             :default-categories ("technology" "clojure")
             :tags-as-categories nil)
            ))))

(provide 'personal/prelude-org)
;;; prelude-org.el ends here
