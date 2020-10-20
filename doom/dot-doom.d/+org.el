;;; ../src/open/dotfiles/doom/dot-doom.d/+org.el -*- lexical-binding: t; -*-

;; =========================================================================
;;                               ORG MODE
;; =========================================================================

;; -------------------------------------------------------------------------
;;                                AGENDA
;; -------------------------------------------------------------------------

(setq org-directory                     "~/doc/notes/"
      org-agenda-files                  '("~/doc/notes/")
      org-agenda-window-setup           'current-window
      org-agenda-include-diary          t
      org-agenda-show-log               t
      org-agenda-skip-deadline-if-done  t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-todo-ignore-deadlines  t
      org-agenda-todo-ignore-scheduled  t
      org-agenda-start-on-weekday       1
      org-agenda-use-tag-inheritance    nil)

;; -------------------------------------------------------------------------
;;                                JOURNAL
;; -------------------------------------------------------------------------

(setq org-journal-encrypt-journal       t
      org-journal-file-format           "%Y%m%d.org")

;; -------------------------------------------------------------------------
;;                               APPEARANCE
;; -------------------------------------------------------------------------

(setq org-ellipsis                       "…"
      org-startup-folded                 nil
      org-startup-indented               t
      org-pretty-entities                t
      org-fontify-done-headline          t
      org-fontify-whole-heading-line     t
      org-fontify-quote-and-verse-blocks t
      org-fontify-emphasized-text        t
      org-src-fontify-natively           t
      org-src-tab-acts-natively          t

      org-superstar-headline-bullets-list
      '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")

      org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)"))

      org-todo-keyword-faces
      '(("TODO"      :foreground "#7c7c75" :weight normal :underline t)
        ("STARTED"   :foreground "#0098dd" :weight normal :underline t)
        ("DONE"      :foreground "#50a14f" :weight normal :underline t)
        ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))

      org-priority-faces
      '((65 :foreground "#e45649")
        (66 :foreground "#da8548")
        (67 :foreground "#0098dd")))

;; -------------------------------------------------------------------------
;;                                 BEHAVIOR
;; -------------------------------------------------------------------------

(setq org-export-html-postamble          nil
      org-hide-emphasis-markers          t
      org-html-validation-link           nil
      org-log-done                       nil
      org-outline-path-complete-in-steps nil
      org-refile-use-cache               t
      org-refile-use-outline-path        t
      org-return-follows-link            t
      org-src-window-setup               'current-window
      org-use-fast-todo-selection        t
      org-use-sub-superscripts           "{}"

      org-refile-targets
      '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))

      org-tag-alist
      '(("work"       . ?w)
        ("personal"   . ?p)
        ("study"      . ?s))

      org-capture-templates
      `(("t" "Todo" entry (file+headline "todos.org" "Todos")
         "* TODO %^{Task} %^G")

        ("j" "Journal Entry" entry (file+datetree "journal.org")
         (file "journal.template")))

      org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 10)))
          (tags-todo "+PRIORITY=\"A\"")
          (tags-todo "work")
          (tags-todo "personal")))
        ("n" "Agenda and all TODOs"
         ((agenda "" ((org-agenda-span 10)))
          (alltodo "")))))
;; -------------------------------------------------------------------------
;;                                   REVEAL
;; -------------------------------------------------------------------------

(setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0/"
      org-re-reveal-title-slide          "%t"
      org-re-reveal-hlevel               2
      org-re-reveal-default-frag-style   'appear
      org-re-reveal-control              t
      org-re-reveal-progress             t
      org-re-reveal-history              nil
      org-re-reveal-center               t
      org-re-reveal-rolling-links        nil
      org-re-reveal-keyboard             t
      org-re-reveal-mousewheel           nil
      org-re-reveal-defaulttiming        nil
      org-re-reveal-fragmentinurl        t
      org-re-reveal-pdfseparatefragments nil
      org-re-reveal-overview             t
      org-re-reveal-klipsify-src         t
      reveal_inter_presentation_links    t)

;; -------------------------------------------------------------------------
;;                                   BABEL
;; -------------------------------------------------------------------------
;; if tangling gives an error about "pdf-info-process-assert-running"
;; re-compile pdf-tools with M-x pdf-tools-install
(after! org
  (progn
    (pdf-loader-install)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure    . t)
       (css        . t)
       (dot        . t)
       (emacs-lisp . t)
       (java       . t)
       (js         . t)
       (makefile   . t)
       (prolog     . t)
       (python     . t)
       (R          . t)
       (ruby       . t)
       (scheme     . t)
       (sed        . t)
       (shell      . t)
       (sql        . t)))))

;; -------------------------------------------------------------------------
;;                                   CAPTURE
;; -------------------------------------------------------------------------

(add-hook! 'org-mode-hook #'turn-on-org-pretty-table-mode)
(add-hook! 'org-mode-hook (lambda () (setq left-margin-width 2 right-margin-width 2)))

(message "Loaded +org configuration")
