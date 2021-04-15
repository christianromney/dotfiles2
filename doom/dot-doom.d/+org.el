;;; ../src/open/dotfiles/doom/dot-doom.d/+org.el -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------
;;                          MARKUP FUNCTIONS
;; -------------------------------------------------------------------------

(defun personal/org-markup-word (theChar)
  (save-excursion
    (backward-word)
    (insert-char theChar)
    (forward-word)
    (insert-char theChar)))

(defun personal/org-italicize-word ()
  (interactive)
  (personal/org-markup-word #x00002F))

(defun personal/org-bold-word ()
  (interactive)
  (personal/org-markup-word #x00002A))

(defun personal/org-code-word ()
  (interactive)
  (personal/org-markup-word #x00007E))

(defun personal/org-underline-word ()
  (interactive)
  (personal/org-markup-word #x00005F))

(defun personal/org-verbatim-word ()
  (interactive)
  (personal/org-markup-word #x00003D))

(defun personal/org-strike-word ()
  (interactive)
  (personal/org-markup-word #x00002B))

;; =========================================================================
;;                               ORG MODE
;; =========================================================================

(use-package! org
  :defer t
  :bind
  (("C-. o b" . #'personal/org-bold-word)
   ("C-. o c" . #'personal/org-code-word)
   ("C-. o i" . #'personal/org-italicize-word)
   ("C-. o s" . #'personal/org-strike-word)
   ("C-. o u" . #'personal/org-underline-word)
   ("C-. o v" . #'personal/org-verbatim-word))
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
        '("𐄘" "𐄗" "𐄖" "𐄕" "𐄔" "𐄓" "𐄒" "𐄑" "𐄐")

        ;; map from default to replacement
        org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?‣)
          (?- . ?–)))

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


;; -------------------------------------------------------------------------
;;                              BIBTEX/ORG-REF
;; -------------------------------------------------------------------------

(use-package! org-ref
  :hook org-mode
  :config
  (setq reftex-default-bibliography
        (list (expand-file-name "bibliography/references.bib" org-directory))

        org-ref-bibliography-notes
        (expand-file-name "bibliography/notes.org" org-directory)

        org-ref-default-bibliography
        (list (expand-file-name "bibliography/references.bib" org-directory))

        org-ref-pdf-directory
        (expand-file-name "bibliography/bibtex-pdfs/" org-directory)

        bibtex-completion-bibliography
        (expand-file-name "bibliography/references.bib" org-directory)

        bibtex-completion-library-path
        (expand-file-name "bibliography/bibtex-pdfs/" org-directory)

        bibtex-completion-notes-path
        (expand-file-name "bibliography/bibtex-notes/" org-directory)

        org-ref-completion-library          'org-ref-ivy-cite
        org-ref-show-broken-links           t
        bibtex-completion-pdf-open-function 'org-open-file
        org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (require 'org-ref-isbn)
  (require 'org-ref-arxiv))

(message "Loaded +org configuration")
