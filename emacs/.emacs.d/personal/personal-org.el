(use-package org
  :ensure t
  :config
  ;; Export
  (require 'ob-clojure)
  (require 'cider)

  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-log-done 'note
        org-return-follows-link t
        org-startup-indented t
        org-html-validation-link nil
        org-export-html-postamble nil
        org-export-backends '(ascii html icalendar latex md)
        org-use-sub-superscripts "{}"
        org-agenda-show-log t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-include-diary t
        org-agenda-files '("~/Dropbox/org/agenda.org"
                           "~/Dropbox/org/notes.org"
                           "~/Dropbox/org/pointslope/business.org"))

  ;; Todo list statuses based on Pivotal Tracker
  (setq org-todo-keywords '((type "TODO" "STARTED" "FINISHED" "DELIVERED" "|" "DONE"))
        org-todo-keyword-faces
        '(("TODO"      . (:background "salmon" :foreground "red" :weight bold))
          ("STARTED"   . (:background "light yellow" :foreground "brown" :weight bold))
          ("FINISHED"  . (:background "light blue" :foreground "dark blue" :weight bold))
          ("DELIVERED" . (:background "orange" :foreground "black" :weight bold))
          ("DONE"      . (:background "light green" :foreground "dark green" :weight bold))))

  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* TODO %^{Task} %^G")

          ("b" "Business" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nSCHEDULED: %^t\n")

          ("d" "Deadline" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nDEADLINE: %^t\n")
          
          ("e" "Emergency" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* STARTED %^{Task}" :clock-in :clock-resume)))
  
  (setq org-tag-alist '(("business" . "?b")
                        ("personal" . "?p")
                        ("pointslope" . "?o")
                        ("kip" . "?k")
                        ("sales" . "?s")
                        ("tech" . "?t")
                        ("education" . "?e")
                        ("basketball" . "?b")
                        ("fishing" . "?f")
                        ("buppy" . "?y")
                        ("randi" . "?r")
                        ("nikki" . "?n")
                        ("mom" . "?m")
                        ("dad" . "?d")
                        ("wes" . "?w")))

  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode)
              (smartparens-mode -1)
              (prelude-off)))
  
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
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t)
  (setq org-bullets-bullet-list
        '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦"
          "◯" "⚪" "⚫" "⚬" "￮" "⊙" "⊚" "∙" "∘")))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(use-package org2blog
  :ensure t
  :config
  (require 'auth-source)
  (let ((credentials  (auth-source-user-and-password "blog.pointslope.com")))
    (setq org2blog/wp-blog-alist
          `(("pointslope"
             :url "https://pointslope.com/blog/xmlrpc.php"
             :username ,(car credentials)
             :password ,(cadr credentials)
             :default-title "New Post"
             :default-categories ("clojure")
             :tags-as-categories nil)))))

(use-package htmlize
  :ensure t)

(require 'prelude-latex)
