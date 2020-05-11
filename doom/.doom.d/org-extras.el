;;; ~/.doom.d/org.el -*- lexical-binding: t; -*-
(setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0/"
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
      reveal_inter_presentation_links t)

 (setq org-agenda-include-diary t
       org-agenda-show-log t
       org-agenda-skip-deadline-if-done t
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-timestamp-if-done t
       org-agenda-start-on-weekday 1
       org-agenda-todo-ignore-deadlines t
       org-agenda-todo-ignore-scheduled t
       org-agenda-use-tag-inheritance nil
       org-agenda-window-setup 'current-window
       org-ellipsis "⤵"
       org-export-html-postamble nil
       org-fontify-done-headline t
       org-html-validation-link nil
       org-log-done nil
       org-outline-path-complete-in-steps nil
       org-pretty-entities t
       org-refile-use-cache t
       org-refile-use-outline-path t
       org-return-follows-link t
       org-src-fontify-natively t
       org-src-tab-acts-natively t
       org-src-window-setup 'current-window
       org-startup-indented t
       org-startup-folded nil
       org-use-fast-todo-selection t
       org-use-sub-superscripts "{}"

       org-bullets-bullet-list '("◉" "○" "◌" "●")

       org-refile-targets
       '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))

       org-tag-alist '(("work"       . ?w)
                       ("personal"   . ?p)
                       ("study"      . ?s))

       org-todo-keywords
       '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)"))

       org-todo-keyword-faces
       '(("TODO"      . (:background "IndianRed1"   :foreground "red4"       :weight bold))
         ("STARTED"   . (:background "gold1"        :foreground "black"      :weight bold))
         ("DONE"      . (:background "light green"  :foreground "dark green" :weight bold)))

       org-capture-templates
       `(("t" "Todo" entry (file+headline "todos.org" "Todos")
          "* TODO %^{Task} %^G")

         ("j" "Journal Entry" entry (file+datetree "journal.org")
          (file "journal.template")))

       org-agenda-custom-commands
       '(("d" "Dashboard"
          ((agenda "" ((org-agenda-span 10)))
           (tags-todo "+PRIORITY=\"A\"")
           (tags-todo "work|reify")
           (tags-todo "personal")))
         ("n" "Agenda and all TODOs"
          ((agenda "" ((org-agenda-span 10)))
           (alltodo "")))))

(after! org-capture
  (progn
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
   See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: "))
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                     ":HUGO_BASE_DIR: ~/src/personal/christianromney.dev/"
                     ":EXPORT_FILE_NAME: index"
                     ":END:"
                     "%?\n")                ;Place the cursor here finally
                   "\n")))
    (setq org-hugo-default-section-directory "posts")
    (add-to-list 'org-capture-templates
                 '("b"                ;; C c n n b
                   "Blog Post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+olp "journal.org" "Website")
                   (function org-hugo-new-subtree-post-capture-template)))))
