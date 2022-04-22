;;; ../src/open/dotfiles/doom/dot-doom.d/+org.el -*- lexical-binding: t; -*-

;; =========================================================================
;;                               ORG MODE
;; =========================================================================
(use-package! org
  :defer t
  :init
  (when (featurep! :lang org +roam2)
    (setq org-roam-directory "~/doc/notes/roam/"))
  :bind
  (("C-. o b" . #'custom/org-bold-word)
   ("C-. o c" . #'custom/org-code-word)
   ("C-. o i" . #'custom/org-italicize-word)
   ("C-. o s" . #'custom/org-strike-word)
   ("C-. o u" . #'custom/org-underline-word)
   ("C-. o v" . #'custom/org-verbatim-word))
  :config
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
        '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")


        ;; map from default to replacement
        org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?‣)
          (?- . ?•))
        )

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
          ("learning"   . ?l))

        org-capture-templates
        `(("t" "Todo" entry (file+headline "todo.org" "Todos")
           "* TODO %^{Task} %^G"))

        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 10)))
            (tags-todo "+PRIORITY=\"A\"")
            (tags-todo "work")
            (tags-todo "personal")))
          ("n" "Agenda and all TODOs"
           ((agenda "" ((org-agenda-span 10)))
            (alltodo "")))))

  (setq plantuml-default-exec-mode 'jar))

;; -------------------------------------------------------------------------
;;                                   HOOKS
;; -------------------------------------------------------------------------

(add-hook! 'org-mode-hook #'org-pretty-table-mode)
(add-hook! 'org-mode-hook (lambda () (setq left-margin-width 2
                                           right-margin-width 2)))

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
       (plantuml   . t)
       (prolog     . t)
       (python     . t)
       (R          . t)
       (ruby       . t)
       (scheme     . t)
       (sed        . t)
       (shell      . t)
       (sql        . t)))))

;; https://www.youtube.com/watch?v=KMlp9HUJI3s&list=PLVtKhBrRV_ZkPnBtt_TD1Cs9PJlU0IIdE&index=24
;; http://www.mkbehr.com/posts/a-research-workflow-with-zotero-and-org-mode/
(use-package! zotxt
  :when (featurep! :tools biblio)
  :after org
  :hook (org-mode . org-zotxt-mode)
  :config
  (setq bibtex-dialect                  'biblatex
        org-cite-csl-styles-dir         "~/doc/notes/zotero/styles/"))

(use-package! websocket
  :when (featurep! :lang org +roam2)
  :after org)

(use-package! org-roam-ui
  :when (featurep! :lang org +roam2)
  :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(message "Loaded +org configuration")
