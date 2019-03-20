;;; romney-org.el --- org mode settings
;;; Commentary:
;;
;;; Code:
(use-package org
  :ensure t
  :defer t
  :pin org
  :mode ("\\.org\\’" . org-mode)
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
  (add-to-list 'org-agenda-files personal-org-file-journal)
  (add-to-list 'org-agenda-files (expand-file-name "math-for-programmers.org" personal-org-dir))
  (add-to-list 'org-agenda-files (expand-file-name "little-typer.org" personal-org-dir))
  (add-to-list 'org-agenda-files (expand-file-name "notes.org" personal-org-dir))
  (setq org-directory personal-org-dir
        org-default-notes-file personal-org-file-default

        org-agenda-include-diary t
        org-agenda-show-all-dates t
        org-agenda-show-log t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-todo-ignore-deadlines t
        org-agenda-todo-ignore-scheduled t
        org-agenda-window-setup 'current-window
        org-babel-clojure-backend 'cider
        org-confirm-babel-evaluate nil
        org-ellipsis "⤵"
        org-export-backends '(ascii html icalendar latex md)
        org-export-html-postamble nil
        org-fontify-done-headline t
        org-html-validation-link nil
        org-log-done nil
        org-outline-path-complete-in-steps nil
        org-refile-use-cache t
        org-refile-use-outline-path t
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-indented t
        org-use-fast-todo-selection t
        org-use-sub-superscripts "{}"

        org-refile-targets
        '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))

        org-tag-alist '(("work"       . ?w)
                        ("personal"   . ?p)
                        ("study"      . ?s)
                        ("clojure"    . ?c)
                        ("racket"     . ?k)
                        ("rust"       . ?r))

        org-todo-keywords
        '((type "TODO" "STARTED" "|" "DONE"))

        org-todo-keyword-faces
        '(("TODO"      . (:background "salmon"       :foreground "red"        :weight bold))
          ("STARTED"   . (:background "yellow"       :foreground "black"      :weight bold))
          ("DONE"      . (:background "light green"  :foreground "dark green" :weight bold)))

        org-capture-templates
        `(("r" "Recipe" entry (file personal-org-file-cookbook)
           "%(org-chef-get-recipe-from-url)" :empty-lines 1)

          ("t" "Task" entry (file+headline personal-org-file-todo "Todos")
           "* TODO %^{Task} %^G")

          ("j" "Journal Entry" entry (file+datetree personal-org-file-journal)
           (file ,personal-org-template-journal))

          ("s" "Study Notes" entry (file+headline personal-org-file-notes "Notes")
           (file ,personal-org-template-note))

          ("q" "Study Quote" entry (file+headline personal-org-file-notes "Notes")
           (file ,personal-org-template-quote))

          ("c" "Code Snippet" entry (file personal-org-file-snippets)
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n%a\n\n#+END_SRC"))

        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 7)))
            (tags-todo "work")
            (tags-todo "personal")))))

  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-fill-mode)))

  (add-hook 'after-init-hook
            (lambda () (org-agenda t "d")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot        . t)
     (shell      . t)
     (clojure    . t)
     (scheme     . t)
     (java       . t)
     (prolog     . t)
     (js         . t)
     (ruby       . t)
     (python     . t)))

  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
  (add-to-list 'org-babel-tangle-lang-exts '("js"      . "js"))

  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(use-package ob-prolog
  :ensure t
  :defer t)

(use-package org-bullets
  :ensure t
  :defines (org-bullets-bullet-list)
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "◌" "●")))

(use-package org-re-reveal
  :ensure t
  :config
  (setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0/js/reveal.min.js"
        org-re-reveal-title-slide "%t"
        org-re-reveal-hlevel 2
        org-re-reveal-default-frag-style 'appear
        org-re-reveal-control t
        org-re-reveal-progress t
        org-re-reveal-history nil
        org-re-reveal-center t
        org-re-reveal-rolling-links nil
        org-re-reveal-keyboard t
        org-re-reveal-mousewheel nil
        org-re-reveal-defaulttiming nil
        org-re-reveal-fragmentinurl t
        org-re-reveal-pdfseparatefragments nil
        org-re-reveal-overview t
        org-re-reveal-klipsify-src t
        reveal_inter_presentation_links t))

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

(use-package org-board
  :ensure t
  :defer t
  :config
  (setq org-attach-directory personal-bookmarks-dir))

(provide 'romney-org)
;;; romney-org.el ends here
