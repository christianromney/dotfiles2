;;; romney-org.el --- org mode settings
;;; Commentary:
;;
;;; Code:
(use-package org
  :ensure t
  :pin org
  :defer t
  :defines
  (org-startup-indented
   org-html-validation-link
   org-export-html-postamble
   org-use-sub-superscripts
   org-agenda-show-log
   org-agenda-todo-ignore-scheduled
   org-agenda-todo-ignore-deadlines
   org-agenda-skip-deadline-if-done
   org-agenda-skip-scheduled-if-done
   org-agenda-include-diary
   org-capture-templates
   org-babel-clojure-backend)
  :config
  (require 'ob-clojure)
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
  (setq org-todo-keywords '((type "TODO" "STARTED" "FINISHED" "DELIVERED" "|" "DONE"))
        org-todo-keyword-faces
        '(("TODO"      . (:background "salmon"       :foreground "red"        :weight bold))
          ("STARTED"   . (:background "light yellow" :foreground "brown"      :weight bold))
          ("FINISHED"  . (:background "light blue"   :foreground "dark blue"  :weight bold))
          ("DELIVERED" . (:background "orange"       :foreground "black"      :weight bold))
          ("DONE"      . (:background "light green"  :foreground "dark green" :weight bold))))

  (setq org-capture-templates
        '(("r" "Recipe" entry (file "~/Dropbox/org/cookbook.org")
           "%(org-chef-get-recipe-from-url)"
           :empty-lines 1)

          ("t" "Task" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* TODO %^{Task} %^G")

          ("b" "Business" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nSCHEDULED: %^t\n")

          ("d" "Deadline" entry (file+headline "~/Dropbox/org/pointslope/business.org" "Todos")
           "* TODO %^{Task} :business:\nDEADLINE: %^t\n")

          ("e" "Emergency" entry (file+headline "~/Dropbox/org/notes.org" "Todos")
           "* STARTED %^{Task}" :clock-in :clock-resume)))

  (setq org-tag-alist '(("business"   . "?b")
                        ("personal"   . "?p")
                        ("tech"       . "?t")
                        ("education"  . "?e")
                        ("basketball" . "?b")))

  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode)))

  (setq org-babel-clojure-backend 'cider
        org-fontify-done-headline t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot        . t)
     (shell      . t)
     (clojure    . t)
     (scheme     . t)
     (java       . t)
     (js         . t)
     (ruby       . t)
     (python     . t)))

  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))
  (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(use-package org-bullets
  :ensure t
  :after org-mode
  :hook org-mode
  :defines (org-bullets-bullet-list)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-bullets-bullet-list
        '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦"
          "◯" "⚪" "⚫" "⚬" "￮" "⊙" "⊚" "∙" "∘")))

;; (use-package ox-reveal
;;   :ensure t
;;   :after org-mode
;;   :defines (org-reveal-root)
;;   :config
;;   (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.4.1/"))

(use-package org-beautify-theme
  :ensure t
  :after org-mode
  :defer t)

(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))

(use-package htmlize
  :ensure t
  :defer t)

(provide 'romney-org)
;;; romney-org.el ends here
