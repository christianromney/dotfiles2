;;; ../src/open/dotfiles/doom/dot-doom.d/+org.el -*- lexical-binding: t; -*-

;; =========================================================================
;;                               ORG MODE
;; =========================================================================

;; from https://takeonrules.com/2022/01/11/resolving-an-unable-to-resolve-link-error-for-org-mode-in-emacs/
(defun custom/org-rebuild-cache ()
  "Rebuild the `org-mode' (and `org-roam') cache(s)."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (when (featurep! :lang org +roam2)
    (org-roam-db-sync)
    (org-roam-update-org-id-locations)))

(use-package! org
  :defer t
  :init
  (when (featurep! :lang org +roam2)
    (setq org-roam-directory         "~/doc/notes/content/roam/"
          org-roam-dailies-directory "journal/"
          org-roam-mode-sections     '((org-roam-backlinks-section :unique t)
                                       org-roam-reflinks-section)
          org-roam-graph-executable  "neato"
          org-roam-capture-templates
          '(("d" "default" plain "%?"
            :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}")
            :unnarrowed t)
            ("s" "sensitive" plain "%?"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                                "#+title: ${title}\n")
             :unnarrowed t))
          org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?"
             :target (file+head "%<%Y-%m-%d>.org.gpg"
                                "#+title: %<%Y-%m-%d>\n")))))
  :bind
  (:map org-mode-map
   ("C-. o b"   . #'custom/org-bold-word)
   ("C-. o c"   . #'custom/org-code-word)
   ("C-. o i"   . #'custom/org-italicize-word)
   ("C-. o s"   . #'custom/org-strike-word)
   ("C-. o u"   . #'custom/org-underline-word)
   ("C-. o v"   . #'custom/org-verbatim-word)
   ("C-c n r b" . #'custom/org-rebuild-cache)
   )

  :config
  ;; -------------------------------------------------------------------------
  ;;                                AGENDA
  ;; -------------------------------------------------------------------------
  (setq org-directory                     "~/doc/notes/content/"
        org-agenda-files                  '("~/doc/notes/content/todo.org.gpg"
                                            "~/doc/notes/content/")
        org-agenda-window-setup           'current-window
        org-agenda-include-diary          t
        org-agenda-show-log               t
        org-agenda-skip-deadline-if-done  t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-todo-ignore-deadlines  t
        org-agenda-todo-ignore-scheduled  t
        org-agenda-start-on-weekday       1
        org-agenda-use-tag-inheritance    nil
        calendar-mark-holidays-flag       t
        calendar-location-name            "Pembroke Pines, FL"
        calendar-latitude                 26.0
        calendar-longitude                -80.3
        calendar-week-start-day           1)

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
        '((nil :maxlevel . 5)
          (org-agenda-files :maxlevel . 5))

        org-tag-alist
        '((:startgrouptag)
          ("study"      . ?s)
          (:grouptags)
          ("book"       . ?b)
          ("paper"      . ?p)
          (:endgrouptag)
          ("work"       . ?w)
          ("personal"   . ?p))

        org-capture-templates
        `(("t" "Todo" entry (file+headline "todo.org.gpg" "Todos")
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

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-priority t)
                                  (:auto-tags t)
                                  (:auto-todo t)))
  (org-super-agenda-mode))

(use-package! holidays
  :after org-agenda
  :config
  (require 'brazilian-holidays)
  (setq calendar-holidays
        (append '((holiday-fixed 1 1   "New Year's Day")
                  (holiday-fixed 2 14  "Valentine's Day")
                  (holiday-fixed 4 1   "April Fools' Day")
                  (holiday-fixed 10 31 "Halloween")
                  (holiday-easter-etc)
                  (holiday-fixed 12 24 "Christmas Eve")
                  (holiday-fixed 12 25 "Christmas")
                  (solar-equinoxes-solstices))
                brazilian-holidays--general-holidays)))

;; -------------------------------------------------------------------------
;;                                   HOOKS
;; -------------------------------------------------------------------------

(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
(add-hook! 'org-mode-hook #'org-modern-mode)
(add-hook! 'org-mode-hook :append #'org-auto-tangle-mode)
(add-hook! 'org-mode-hook :append (lambda () (setq left-margin-width 2
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
(use-package! graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 2))

;; if tangling gives an error about "pdf-info-process-assert-running"
;; re-compile pdf-tools with M-x pdf-tools-install
(after! org
  (setq org-auto-tangle-default t)
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

;; -------------------------------------------------------------------------
;;                               BIBLIOGRAPHY
;; -------------------------------------------------------------------------
;; w/ Zotero
;; https://www.youtube.com/watch?v=KMlp9HUJI3s
;; http://www.mkbehr.com/posts/a-research-workflow-with-zotero-and-org-mode/
(use-package! zotxt
  :after org
  :hook (org-mode . org-zotxt-mode)
  :config
  (setq bibtex-dialect                  'biblatex
        org-cite-csl-styles-dir         "~/doc/notes/zotero/styles/"))

(message "Loaded +org configuration")
