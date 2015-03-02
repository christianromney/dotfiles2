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

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode)))

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

;; Credentials are encrypted in ~/.authinfo.gpg
;; and Emacs can *just read them* if GPG agent is running
(require 'org2blog-autoloads)
(require 'netrc)

;; 't will use shortcodes / plugins, nil will use Emacs native
(setq org2blog/wp-use-sourcecode-shortcode 't)

;; Nothing to add...
(setq org2blog/wp-sourcecode-default-params nil)

;; Syntax highlighting to support
(setq org2blog/wp-sourcecode-langs
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
          )))

(provide 'prelude-org)
;;; prelude-org.el ends here
