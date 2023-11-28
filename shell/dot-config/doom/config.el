(message "> Initializing Emacs...")
(setq user-full-name    "Christian Romney"
      user-mail-address "christian.a.romney@gmail.com")

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode
      initial-major-mode              'lisp-interaction-mode
      inhibit-startup-message         t
      display-line-numbers-type       nil)

(setq confirm-kill-emacs              nil
      use-short-answers               t
      enable-dir-local-variables      t
      enable-local-variables          t
      kill-buffer-query-functions     (remq 'process-kill-buffer-query-function
                                            kill-buffer-query-functions))

(setq native-comp-async-report-warnings-errors 'silent)

;; default frame settings
(setq default-frame-alist
      '((fullscreen . maximized)))

(setq doom-cache-dir user-emacs-directory)
(setq +file-templates-dir (expand-file-name "snippets" doom-private-dir))
(setq +default-want-RET-continue-comments nil)

(add-to-list 'doom-large-file-size-alist
             '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

(display-line-numbers-mode -1)

(message "...global behavor...")

(setq doom-theme 'doom-tomorrow-day
      doom-font (font-spec :family "JetBrains Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Metropolis" :size 18)
      doom-serif-font (font-spec :family "Times New Roman" :size 20)
      doom-themes-enable-bold     t
      doom-themes-enable-italic   t
      doom-themes-padded-modeline t)

(doom-themes-visual-bell-config)
(doom-themes-org-config)

(setq-default tab-width 2)
(setq-default cursor-type 'bar)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; faces
(face-spec-set 'doom-themes-visual-bell
                '((default :weight normal
                   :background "firebrick2"
                   :foreground "white")))
(setq face-remapping-alist
      '((show-paren-match . (:inherit pulsar-yellow)) ;; yellow highlight
       (show-paren-mismatch . (:inherit flycheck-error))))

;; double rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(message "...appearance...")

(setq abbrev-file-name (expand-file-name  "etc/abbrev_defs" doom-private-dir)
      save-abbrevs     'silent)

(setq-default abbrev-mode t)

(setq bookmark-default-file     (expand-file-name "etc/bookmarks" doom-private-dir)
      bookmark-old-default-file bookmark-default-file
      bookmark-file             bookmark-default-file
      bookmark-sort-flag        t)

(after! dired
  (add-hook 'dired-mode-hook #'diredfl-mode)
  (map!
   :map dired-mode-map
   "C-l" #'dired-up-directory)
  (when IS-MAC
    (setq insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches   "-aBhl --group-directories-first")
    (map!
     :map dired-mode-map
     "r"  #'reveal-in-osx-finder)))
 (message "...built-in modes...")

(defun cr/mkdirp (path)
  "Ensures the directory path exists, creating any parents as
needed. Returns the expanded pathname."
  (let ((abspath (expand-file-name path)))
    (if (file-exists-p abspath)
        abspath
      (progn
        (make-directory abspath 'parents)
        abspath))))

(defun cr/touch (path)
  "Ensures the file path exists, creating any parents as needed.
Returns the expanded pathname."
  (let ((abspath (expand-file-name path)))
    (if (file-exists-p abspath)
        abspath
      (progn
        (make-empty-file abspath 'parents)
        abspath))))

(defun cr/read-file-as-string (path)
  "Reads the given file as a string."
  (string-trim
   (with-temp-buffer
     (insert-file-contents (expand-file-name path))
     (buffer-string))))

(defun cr/keychain-api-token-for-host (host)
  "Reads the keychain internet password for the given host."
  (string-trim
   (shell-command-to-string
    (string-join `("security find-internet-password -s " ,host " -w") ""))))

(defun cr/port-open-p (port)
  "Returns t if the given port is in use, nil otherwise."
  (= 0 (call-process "lsof" nil nil nil "-P" "-i"
                     (concat "TCP:" (number-to-string port)))))

(defun cr/read-auth-field (field &rest params)
  (require 'auth-source)
  (let ((match (car (apply #'auth-source-search params))))
    (if match
        (let ((secret (plist-get match field)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "%s not found for %S" field params))))

(defun cr/read-auth-username (&rest params)
  (apply #'cr/read-auth-field :user params))

(defun cr/read-auth-password (&rest params)
  (apply #'cr/read-auth-field :secret params))

(defun cr/just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun cr/delete-horizontal-space ()
  "Command to delete all whitespace. Depends on smartparens, which
Doom loads early."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(defun cr/temperature-conversions (num)
  "Interprets the given num as farenheit and celsius degrees and
returns the conversion of each to the other. "
  (let ((celsius (* 5.0 (/ (- num 32.0) 9.0)))
        (farenheit (+ 32.0 (* 9.0 (/ num 5.0)))))
     `((farenheit . ,farenheit)
       (celsius . ,celsius))))

(defun cr/message-temperature-conversions (num)
  "Interprets the given num as farenheit and celsius degrees and
displays the conversions of each to the other in the echo area."
  (let* ((temps (cr/temperature-conversions num))
         (degf  (alist-get 'farenheit temps))
         (degc  (alist-get 'celsius temps)))
    (message "Temperatures: %2.1f℃ => %2.1f℉; %2.1f℉ => %2.1f℃"
             num degf num degc)))

(defun cr/display-temperature-at-point-conversions ()
  "Displays the number at point as both farenheit and celsius
degrees in the echo area."
  (interactive)
  (when-let ((num (number-at-point)))
    (cr/message-temperature-conversions num)))

(message "...custom elisp functions...")

;; main directory
(defvar +info-dir "~/Documents/personal/notes"
  "The root for all notes, calendars, agendas, todos, attachments, and bibliographies.")

(setq org-directory              (expand-file-name "content" +info-dir)
      org-clock-persist-file     (expand-file-name "etc/org-clock-save.el" doom-cache-dir))

;; roam notes
(setq org-roam-directory         (expand-file-name "roam" org-directory)
      org-roam-dailies-directory "journal/"
      org-roam-db-location       (expand-file-name ".org-roam.db" org-directory ))

;; agenda
(setq org-agenda-file-regexp              "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"
      org-agenda-files                   (list org-directory
                                               org-roam-directory
                                               org-roam-dailies-directory)
      org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-directory))

;; capture
(setq +org-capture-changelog-file "changelog.org"
      +org-capture-notes-file     "notes.org"
      +org-capture-projects-file  "projects.org"
      +org-capture-todo-file      "todo.org"
      +org-capture-journal-file   "journal.org")

(message "...org directories and files...")

(defun cr/markup-word (markup-char)
  "Wraps the active region or the word at point with MARKUP-CHAR."
  (cl-destructuring-bind (text start end)
      (if (use-region-p)
          (list
           (buffer-substring-no-properties (region-beginning) (region-end))
           (region-beginning)
           (region-end))
        (let ((bounds (bounds-of-thing-at-point 'word)))
          (list (thing-at-point 'word)
                (car bounds)
                (cdr bounds))))
    (save-excursion
      (replace-region-contents
       start end
       (lambda ()
         (s-wrap text
                 (char-to-string markup-char)
                 (char-to-string markup-char)))))))

(defun cr/org-italicize-word ()
  (interactive)
  (cr/markup-word #x00002F))

(defun cr/org-bold-word ()
  (interactive)
  (cr/markup-word #x00002A))

(defun cr/org-code-word ()
  (interactive)
  (cr/markup-word #x00007E))

(defun cr/org-underline-word ()
  (interactive)
  (cr/markup-word #x00005F))

(defun cr/org-verbatim-word ()
  (interactive)
  (cr/markup-word #x00003D))

(defun cr/org-strike-word ()
  (interactive)
  (cr/markup-word #x00002B))

(message "...org custom markup functions...")

;; which modules to load
(setq org-modules
      '(ol-bibtex
        ol-bookmark
        org-checklist
        ol-docview
        ol-doi
        org-id
        org-tempo))

(after! org
  ;; startup configuration
  (setq org-startup-with-inline-images t
        org-startup-with-latex-preview nil
        org-M-RET-may-split-line       t)

  ;; behaviors
  (setq org-export-html-postamble          nil
        org-hide-emphasis-markers          t
        org-html-validation-link           nil
        org-log-done                       nil
        org-outline-path-complete-in-steps nil
        org-return-follows-link            t
        org-src-window-setup               'current-window
        org-use-fast-todo-selection        t
        org-use-sub-superscripts           "{}")

  ;; agenda
  (setq org-agenda-tags-column            0
        org-agenda-block-separator        ?─
        org-agenda-window-setup           'current-window
        org-agenda-include-diary          t
        org-agenda-show-log               t
        org-agenda-skip-deadline-if-done  t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-start-on-weekday       1
        org-agenda-todo-ignore-deadlines  t
        org-agenda-todo-ignore-scheduled  t
        org-agenda-use-tag-inheritance    nil
        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 10)))
            (tags-todo "+PRIORITY=\"A\"")
            (tags-todo "work")
            (tags-todo "personal")))
          ("n" "Agenda and all TODOs"
           ((agenda "" ((org-agenda-span 10)))
            (alltodo ""))))
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")

  ;; refiling
  (setq org-refile-use-cache                   t ;; use C-0 C-c C-w to clear cache
        org-refile-use-outline-path            t
        org-refile-allow-creating-parent-nodes t
        org-refile-targets                     '((nil :maxlevel . 5)
                                                 (org-agenda-files :maxlevel . 5)))
  ;; capture
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline "todo.org" "Todos")
           "* TODO %^{Task} %^G")))

  ;; todos
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WIP(w)" "WAIT(a)" "PAUSE(p)" "|" "DONE(d)" "KILL(k)" "ASSIGNED(a)")))

  ;; tags
  (setq org-tag-alist
        '((:startgrouptag)
          ("study"      . ?s)
          (:grouptags)
          ("book"       . ?b)
          ("paper"      . ?a)
          (:endgrouptag)
          (:startgrouptag)
          ("work"       . ?w)
          ("personal"   . ?m)
          ("FLAGGED"    . ?f)))

  ;; visual appearance
  (setq org-ellipsis                       "…"
        org-fontify-done-headline          t
        org-fontify-emphasized-text        t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line     t
        org-pretty-entities                t
        org-hide-emphasis-markers t
        org-src-fontify-natively           t
        org-src-tab-acts-natively          t
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-startup-folded                 t
        org-startup-indented               t)

  ;; add frame borders and window dividers
  ;; (modify-all-frames-parameters
  ;;  '((right-divider-width . 40)
  ;;    (internal-border-width . 40)))

  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))

  ;; change faces
  (face-spec-set 'org-agenda-date
                 '((default :weight normal)))
  (face-spec-set 'org-agenda-date-weekend
                 '((default :foreground "#399ee6" :weight normal)))
  (face-spec-set 'org-agenda-diary
                 '((default :weight normal :foreground "#86b300")))
  (face-spec-set 'org-agenda-date-today
                 '((default :foreground "#f07171" :slant italic :weight normal)))
  (face-spec-set 'org-modern-tag
                 '((default :weight normal :background "#d1bce5")))
  (set-face-background 'fringe (face-attribute 'default :background))

  ;; keybindings
  (map!
   (:map org-mode-map
    :desc "org markup"
    :prefix ("C-, o" . "org markup word")
    :desc "bold"            "b" #'cr/org-bold-word
    :desc "code"            "c" #'cr/org-code-word
    :desc "italics"         "i" #'cr/org-italicize-word
    :desc "strikethrough"   "s" #'cr/org-strike-word
    :desc "underline"       "u" #'cr/org-underline-word
    :desc "verbatim"        "v" #'cr/org-verbatim-word

    )))
(message "...org startup, bindings, agenda, tags, todos...")

;; org-modern-star (appearance)
(after! org
  (setq org-modern-star
        '("◉" "○" "▣" "□" "◈" "◇" "✦" "✧" "✻" "✾"))
  (global-org-modern-mode))
(message "...org appearance...")

(defface +calendar-holiday
  '((t . (:inherit pulsar-cyan)))
  "Face for holidays in calendar.")

(defface +calendar-today
  '((t . (:foreground "violet red" :box t)))
  "Face for the current day in calendar.")

(defface +calendar-appointment
  '((t . (:inherit pulsar-yellow)))
  "Face for appointment diary entries in calendar.")

(after! org
  (require 'brazilian-holidays)
  (setq calendar-location-name               "Pembroke Pines, FL"
        calendar-latitude                    26.0
        calendar-longitude                   -80.3
        calendar-week-start-day              0
        calendar-mark-holidays-flag          t
        calendar-mark-diary-entries-flag     t
        calendar-christian-all-holidays-flag nil
        calendar-holiday-marker              '+calendar-holiday
        calendar-today-marker                '+calendar-today
        diary-entry-marker                   '+calendar-appointment
        cal-html-directory                   "~/Desktop"
        cal-html-holidays                    t
        diary-file
        (expand-file-name "appointment-diary" org-directory)

        calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays
                holiday-christian-holidays
                holiday-solar-holidays
                brazilian-holidays--general-holidays
                brazilian-holidays-sp-holidays))
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today))
  (message "...org calendar...")

(use-package! org-glossary
  :hook org-mode
  :init
  (defface org-glossary-term
  '((default :inherit (popup-tip-face)
     :weight normal))
  "Base face used for term references.")
  :config
  (setq org-glossary-fontify-types-differently nil)
  (map!
   (:map org-mode-map
    :prefix ("C-c y" . "glossary")
    :desc "define term"     "d" #'org-glossary-create-definition
    :desc "goto definition" "g" #'org-glossary-goto-term-definition)))

(message "...org glossary...")

(after! org
  (when (modulep! :tools biblio)
    (setq! citar-bibliography
           (list (expand-file-name "references.bib" +info-dir))))
  (setq bibtex-dialect                  'biblatex
        org-cite-csl-styles-dir         (expand-file-name "zotero/styles/" +info-dir))
  (add-hook 'org-mode-hook #'org-zotxt-mode))

(message "...org citations, zotero, citar...")

(after! org
  (setq org-auto-tangle-default t)
  (add-hook 'org-mode-hook #'org-auto-tangle-mode))

(use-package! graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 2))

(after! org
  (when (modulep! :lang plantuml)
    (setq plantuml-default-exec-mode 'jar))

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
     (sql        . t))))

(message "...org babel...")

(after! org
  (setq reveal_inter_presentation_links    t
        org-re-reveal-center               t
        org-re-reveal-control              t
        org-re-reveal-default-frag-style   'appear
        org-re-reveal-defaulttiming        nil
        org-re-reveal-fragmentinurl        t
        org-re-reveal-history              nil
        org-re-reveal-hlevel               2
        org-re-reveal-keyboard             t
        org-re-reveal-klipsify-src         t
        org-re-reveal-mousewheel           nil
        org-re-reveal-overview             t
        org-re-reveal-pdfseparatefragments nil
        org-re-reveal-progress             t
        org-re-reveal-rolling-links        nil
        org-re-reveal-title-slide          "%t"
        org-re-reveal-root
        "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.5.0/reveal.js"))

(message "...org reveal...")

(map! "<s-left>"  #'sp-forward-barf-sexp
      "<s-right>" #'sp-forward-slurp-sexp
      "C-'"       #'avy-goto-line
      "C-:"       #'avy-goto-char
      "C-M-%"     #'anzu-query-replace-regexp
      "C-c M-t"   #'transpose-sentences
      "C-c a"     #'org-agenda
      "C-c g"     #'google-this
      "C-e"       #'move-end-of-line
      "C-x M-s"   #'transpose-sexps
      "C-x M-t"   #'transpose-paragraphs
      "C-x P"     #'print-buffer
      "C-x \\"    #'align-regexp
      "C-x g"     #'magit-status
      "C-x r I"   #'string-insert-rectangle
      "C-x t c"   #'cr/display-temperature-at-point-conversions
      "M-%"       #'anzu-query-replace
      "M-/"       #'hippie-expand
      "M-SPC"     #'cr/just-one-space
      "M-\\"      #'cr/delete-horizontal-space
      "M-o"       #'other-window
      "M-p"       #'fill-paragraph)

(message "...global keybindings...")

(when (modulep! :completion vertico)
  (use-package! vertico
    :demand t
    :defer t
    :bind
    (("C-x B"    . #'+vertico/switch-workspace-buffer)
     :map vertico-map
     ("C-l"      . #'vertico-directory-up)) ;; behave like helm to go up a level
    :config
    (setq vertico-cycle t
          read-extended-command-predicate #'command-completion-default-include-p
          orderless-matching-styles     '(orderless-literal
                                          orderless-initialism
                                          orderless-regexp)
          completion-category-defaults  '((email (styles substring)))
          completion-category-overrides '((file (styles +vertico-basic-remote
                                                        orderless
                                                        partial-completion)))

          marginalia-align              'right))

  (use-package! consult
    :defer t
    :config
    (setq consult-grep-args
          "ggrep --null --line-buffered --color=never --ignore-case \
--exclude-dir=.git --line-number -I -r .")
    :bind
    (("M-i"     . #'consult-imenu)
     ("C-c M-o" . #'consult-multi-occur)
     ("C-x b"   . #'consult-buffer)
     ("C-x 4 b" . #'consult-buffer-other-window)
     ("C-x 5 b" . #'consult-buffer-other-frame)
     ("C-x r b" . #'consult-bookmark)
     ("M-g g"   . #'consult-goto-line)
     ("C-c s r" . #'consult-ripgrep)
     ("C-x r i" . #'consult-register-load)
     ("C-x r s" . #'consult-register-store)
     ("C-h W"   . #'consult-man)
     ("M-s g"   . #'consult-git-grep)))

  (use-package! embark
    :defer t
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("M-." . embark-dwim)        ;; good alternative: M-.
     ) ;; alternative for `describe-bindings'

  ;; Optionally replace the key help with a completing-read interface
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package! embark-consult
  :defer t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)))

(when (modulep! :completion company)
  (use-package! company
    :defer t
    :config
    (setq company-idle-delay 0.5)))
(message "...completion...")

(use-package! pulsar
  :defer t
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.065
        pulsar-iterations 9
        pulsar-face 'pulsar-green
        pulsar-highlight-face 'pulsar-red)
  (pulsar-global-mode t)
  :config
  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

  ;; integration with the built-in `isearch':
  (add-hook 'isearch-mode-end-hook #'pulsar-recenter-middle)
  (advice-add 'isearch-forward :after #'pulsar-recenter-middle)
  (advice-add 'isearch-repeat-forward :after #'pulsar-recenter-middle)
  (advice-add 'isearch-backward :after #'pulsar-recenter-middle)
  (advice-add 'isearch-repeat-backward :after #'pulsar-recenter-middle)

  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry))

(when (modulep! :checkers spell)
  (setq spell-fu-directory
        (cr/mkdirp (expand-file-name "etc/spell-fu/" doom-private-dir)))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary
                "en-personal"
                (expand-file-name "aspell.en.pws" spell-fu-directory))))))

(message "...spell checking...")

(setq blink-matching-paren t
      show-paren-mode t
      show-paren-style 'parenthesis
      show-paren-delay 0)

(pcase-dolist (`(,open . ,close) '(("(" . ")")
                                     ("[" . "]")
                                     ("{" . "}")))
    ;; remove all default rules
    (sp-pair open close :post-handlers nil :unless nil)
    ;; add sole exception
    (sp-pair open close :unless '(:add sp-in-string-p)))

(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook! 'doom-first-buffer-hook #'smartparens-global-mode)

(message "...smartparens...")

(after! projectile
  (cr/mkdirp (expand-file-name "projectile" doom-cache-dir))

  (setq projectile-cache-file
        (expand-file-name "projectile/projectile.cache" doom-cache-dir)
        projectile-known-projects-file
        (expand-file-name "projectile/projectile.projects" doom-cache-dir))

  (pushnew! projectile-project-root-files "project.clj" "deps.edn"))

(message "...projectile...")

(after! magit
  (setq magit-revision-show-gravatars t
        forge-database-file
        (expand-file-name "forge/forge-database.sqlite" doom-cache-dir)
        magit-no-confirm '(stage-all-changes unstage-all-changes)))

(message "...magit...")

(use-package! clojure-mode
  :defer t
  :hook (clojure-mode . rainbow-delimiters-mode)
  :config
  (when (modulep! :tools lsp)
    (map! :map clojure-mode-map
          "C-c j d"    #'lsp-ui-doc-glance
          "C-c j i"    #'lsp-ui-imenu)
    (add-hook! '(clojure-mode-local-vars-hook
                 clojurec-mode-local-vars-hook
                 clojurescript-mode-local-vars-hook)
      (defun +clojure-disable-lsp-indentation-h ()
        (setq-local lsp-enable-indentation nil))
      #'lsp!)
    (after! lsp-clojure
      (dolist (m '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode))
        (add-to-list 'lsp-language-id-configuration (cons m "clojure")))
      (dolist (dir '("[/\\\\]\\.clj-kondo\\'"
                     "[/\\\\]\\.cp-cache\\'"
                     "[/\\\\]\\.lsp\\'"
                     "[/\\\\]\\.shadow-cljs\\'"
                     "[/\\\\]\\target\\'"))
        (add-to-list 'lsp-file-watch-ignored dir)))
    (setq lsp-lens-enable          t       ;; enable LSP code lens for inline reference counts
          lsp-file-watch-threshold 2000
          lsp-enable-snippet       t)))

(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojure-mode-hook :append #'subword-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurec-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurex-mode-hook #'turn-on-smartparens-strict-mode)

(message "...clojure editing...")

(defun +inf-clojure-run-tests ()
  "Run clojure.test suite for the current namespace."
  (interactive)
  (comint-proc-query (inf-clojure-proc)
                        "(clojure.test/run-tests)\n"))

(defun +inf-clojure-pretty-print ()
  "Pretty print the last repl output"
  (interactive)
  (comint-proc-query (inf-clojure-proc)
                     "(do \n(newline)\n(clojure.pprint/pprint *1))\n"))

(defun +inf-clojure-load-file ()
  "Send a load-file instruction to Clojure to load the current file.
Uses comint-proc-query instead of comint-send-string like
inf-clojure does by default, as that method breaks REPLs for me
with large files for some reason."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (comint-proc-query
     (inf-clojure-proc)
     (format "(do (load-file \"%s\") :loaded)\n" file-name))
    (message "inf-clojure :: Loaded file: %s" file-name)))

(defun +possible-project-file (relative-path)
  (if (not (string-blank-p (projectile-project-root)))
      (let ((path (expand-file-name (concat (projectile-project-root) relative-path))))
        (if (file-exists-p path) path nil))
    nil))

(defun +inf-clojure-socket-repl-connect ()
  (interactive)
  (message "inf-clojure-socket-repl-connect in project %s" (projectile-project-root))
  (let ((default-socket-repl-port 5555)
        (found-port-file (+possible-project-file ".shadow-cljs/socket-repl.port")))
    (cond
     ;; option 1: check for shadow-cljs ephemeral port file
     (found-port-file
      (let ((port (cr/read-file-as-string found-port-file)))
        (message "Connecting clojure socket REPL on ephemeral shadow port %s" port)
        (inf-clojure (cons "localhost" port))))

     ;; option 2: check default port
     ((cr/port-open-p default-socket-repl-port)
      (progn
        (message "Connecting clojure socket REPL on detected open port %d" default-socket-repl-port)
        (inf-clojure (cons "localhost" default-socket-repl-port))))

     ;; option 3: ask me
     (t
      (progn
        (message "Connecting clojure socket REPL interactively")
        (inf-clojure-connect))))))

(defun +inf-clojure-reconfigure ()
  (progn
    (message "Setting clojure completion mode to compliment")
    (inf-clojure-update-feature
     'clojure 'completion
     "(compliment.core/completions \"%s\")")))

(use-package! inf-clojure
  :defer t
  :after clojure
  :config
  (map! :map clojure-mode-map
        "C-c c p"    #'+inf-clojure-pretty-print
        "C-c r c"    #'+inf-clojure-socket-repl-connect
        "C-c j c"    #'inf-clojure
        "C-c j C"    #'inf-clojure-connect
        "C-c j D"    #'inf-clojure-show-var-documentation
        "C-c j e b"  #'inf-clojure-eval-buffer
        "C-c j e d"  #'inf-clojure-eval-defun
        "C-c j e D"  #'inf-clojure-eval-defun-and-go
        "C-c j e f"  #'inf-clojure-eval-last-sexp
        "C-c j e F"  #'inf-clojure-eval-form-and-next
        "C-c j e r"  #'inf-clojure-eval-region
        "C-c j e R"  #'inf-clojure-eval-region-and-go
        "C-c j a"    #'inf-clojure-apropos
        "C-c j l"    #'inf-clojure-arglists
        "C-c j m"    #'inf-clojure-macroexpand
        "C-c j r"    #'inf-clojure-reload
        "C-c j R"    #'inf-clojure-restart
        "C-c j v"    #'inf-clojure-show-ns-vars
        "C-c j t"    #'+inf-clojure-run-tests
        "C-c M-j"    #'+inf-clojure-socket-repl-connect
        "C-c C-q"    #'inf-clojure-quit
        "C-c M-n"    #'inf-clojure-set-ns
        "C-c M-p"    #'+inf-clojure-pretty-print
        "C-c C-e"    #'inf-clojure-eval-last-sexp
        "C-x C-e"    #'inf-clojure-eval-last-sexp
        "C-c C-z"    #'inf-clojure-switch-to-repl
        "C-c C-k"    #'+inf-clojure-load-file
        "C-c ,"      #'inf-clojure-clear-repl-buffer
        :map inf-clojure-mode-map
        "C-c ,"      #'inf-clojure-clear-repl-buffer
        "C-c j R"    #'inf-clojure-restart))

(add-hook! 'inf-clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'inf-clojure-mode-hook #'+inf-clojure-reconfigure)

(message "...clojure REPL...")

(when (modulep! :checkers syntax)
  (use-package! flycheck-clj-kondo
    :defer t
    :when (modulep! :checkers syntax)
    :after flycheck))

(message "...clojure static analysis...")

(when (modulep! :lang scheme)
  (add-hook! 'scheme-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook! 'scheme-mode-hook (lambda () (require 'xscheme)))
  (map! :map scheme-mode-map
        "C-c C-b" #'xscheme-send-buffer
        "C-c C-e" #'xscheme-send-previous-expression
        "C-c C-r" #'xscheme-send-region
        "C-c C-z" #'xscheme-select-process-buffer
        "C-c C-c" #'xscheme-send-control-g-interrupt
        "C-c I x" #'xscheme-send-control-x-interrupt
        "C-c I u" #'xscheme-send-control-u-interrupt
        "C-c I b" #'xscheme-send-breakpoint-interrupt
        "C-c I p" #'xscheme-send-proceed)

(message "... scheme..."))

(when (modulep! :lang cc)
  (map! :map c-mode-base-map
        ;; disassembler (objdump)
        "C-c o a"    #'disaster)

  ;; disassembler
  (use-package! disaster
    :defer t
    :commands (disaster)
    :init
    (setq disaster-assembly-mode 'nasm-mode)
    :config
    ;; the default -M att argument doesn't work for me using
    ;; Apple clang version 12.0.5 (clang-1205.0.22.9)
    ;; Target: x86_64-apple-darwin20.4.0
    (setq disaster-objdump "objdump -d -Sl --no-show-raw-insn"))
(message "...C..."))

(progn
  (require 'openai)

  (setq openai-key (cr/keychain-api-token-for-host "api.openai.com"))
  (when (cr/port-open-p 3005)
    (setq openai-base-url "http://0.0.0.0:3005/v1"))

(message "...openai..."))

(use-package! gptel
  :after openai
  :init
  (map! :desc "ChatGPT" "C-c M-h c" #'gptel)
  :config
  (setq gptel-api-key openai-key
        gptel-model "gpt-4-1106-preview")
  (when (cr/port-open-p 3005)
    (setq gptel-openai-endpoint "http://0.0.0.0:3005/v1"
          gptel-stream nil)))

(message "...ChatGPT...")

(use-package! codegpt
  :after openai
  :init
  (require 'codegpt)
  :config
  (setq codegpt-tunnel 'chat
        codegpt-model "gpt-3.5-turbo")
  (map!
   :prefix ("C-c M-h o" . "coding assistant")
   :desc "CodeGPT"        "g" #'codegpt
   :desc "Document code"  "d" #'codegpt-doc
   :desc "Explain code"   "e" #'codegpt-explain
   :desc "Fix code"       "f" #'codegpt-fix
   :desc "Improve code"   "i" #'codegpt-improve))
(message "...CodeGPT...")

(after! org
  (require 'org-ai)
  (setq org-ai-openai-api-token (cr/keychain-api-token-for-host "api.openai.com"))
  (add-to-list 'org-structure-template-alist '("ai" . "ai"))
  (org-ai-install-yasnippets)
  (when (cr/port-open-p 3005)
    (message "=> using openai proxy")
    (setq org-ai-openai-chat-endpoint "http://0.0.0.0:3005/v1/chat/completions"
          org-ai-openai-completion-endpoint "http://0.0.0.0:3005/v1/completions"
          org-ai-on-project-use-stream nil))
  (add-hook 'org-mode #'org-ai-mode)
  (org-ai-global-mode)

(message "...org-ai..."))

(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

(message "> Emacs initialization complete.")
