(setq user-full-name    "Christian Romney"
      user-mail-address "christian.a.romney@gmail.com")

(use-package ef-themes
  :init
  (setq doom-font                 "JetBrains Mono:pixelsize=20"
        display-line-numbers-type t
        fancy-splash-image        (concat doom-private-dir "splash.png"))

  (setq-default tab-width 2)

  ;; theme selection
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-light :no-confirm)

  ;; ligature support
  (mac-auto-operator-composition-mode)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(setq +doom-dashboard-menu-sections
      '(("Reload last session"
         :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
         :when (cond ((modulep! :ui workspaces)
                      (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                     ((require 'desktop nil t)
                      (file-exists-p (desktop-full-file-name))))
         :face (:inherit (doom-dashboard-menu-title bold))
         :action doom/quickload-session)
        ("Open org-agenda"
         :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
         :when (fboundp 'org-agenda)
         :action org-agenda)
        ("Open mail"
         :icon (all-the-icons-faicon "inbox" :face 'doom-dashboard-menu-title)
         :when (modulep! :ui doom-dashboard)
         :action =mu4e)
        ("Recently opened files"
         :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
         :action recentf-open-files)
        ("Open project"
         :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
         :action projectile-switch-project)
        ("Jump to bookmark"
         :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
         :action bookmark-jump)
        ("Open private configuration"
         :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
         :when (file-directory-p doom-user-dir)
         :action doom/open-private-config)
        ("Open documentation"
         :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
         :action doom/help)))

(defun +mkdirp (path)
  "Ensures the directory path exists, creating any parents as
needed. Returns the expanded pathname."
  (let ((abspath (expand-file-name path)))
    (if (file-exists-p abspath)
        abspath
      (progn
        (make-directory abspath 'parents)
        abspath))))

(defun +touch (path)
  "Ensures the file path exists, creating any parents as needed.
Returns the expanded pathname."
  (let ((abspath (expand-file-name path)))
    (if (file-exists-p abspath)
        abspath
      (progn
        (make-empty-file abspath 'parents)
        abspath))))

(defun custom/read-file-as-string (path)
  "Reads the given file as a string."
  (string-trim
   (with-temp-buffer
     (insert-file-contents (expand-file-name path))
     (buffer-string))))

(defun custom/port-open-p (port)
  "Returns t if the given port is in use, nil otherwise."
  (= 0 (call-process "lsof" nil nil nil "-P" "-i"
                     (concat "TCP:" (number-to-string port)))))

(defun custom/read-auth-field (field &rest params)
  (require 'auth-source)
  (let ((match (car (apply #'auth-source-search params))))
    (if match
        (let ((secret (plist-get match field)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "%s not found for %S" field params))))

(defun custom/read-auth-username (&rest params)
  (apply #'custom/read-auth-field :user params))

(defun custom/read-auth-password (&rest params)
  (apply #'custom/read-auth-field :secret params))

(defun custom/just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun custom/delete-horizontal-space ()
  "Command to delete all whitespace. Depends on smartparens, which
Doom loads early."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(defun temperature-conversions (num)
  "Interprets the given num as farenheit and celsius degrees and
returns the conversion of each to the other. "
  (let ((celsius (* 5.0 (/ (- num 32.0) 9.0)))
        (farenheit (+ 32.0 (* 9.0 (/ num 5.0)))))
     `((farenheit . ,farenheit)
       (celsius . ,celsius))))

(defun message-temperature-conversions (num)
  "Interprets the given num as farenheit and celsius degrees and
displays the conversions of each to the other in the echo area."
  (let* ((temps (temperature-conversions num))
         (degf  (alist-get 'farenheit temps))
         (degc  (alist-get 'celsius temps)))
    (message "Temperatures: %2.1f℃ => %2.1f℉; %2.1f℉ => %2.1f℃"
             num degf num degc)))

(defun display-temperature-at-point-conversions ()
  "Displays the number at point as both farenheit and celsius
degrees in the echo area."
  (interactive)
  (when-let ((num (number-at-point)))
    (message-temperature-conversions num)))

(setq confirm-kill-emacs          nil
      enable-dir-local-variables  t
      enable-local-variables      t
      initial-major-mode          'lisp-interaction-mode
      user-emacs-directory        (+mkdirp (expand-file-name "~/.local/emacs/cache/"))
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                            kill-buffer-query-functions))

(setq +default-want-RET-continue-comments nil
      +file-templates-dir                 (expand-file-name "snippets" doom-private-dir)
      doom-cache-dir                      user-emacs-directory)

(add-to-list 'doom-large-file-size-alist
             '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

(setq abbrev-file-name "~/.doom.d/abbrev_defs"
      save-abbrevs     'silent)
(setq-default abbrev-mode t)

(setq bookmark-default-file     (+touch
                                 (expand-file-name "etc/bookmarks" doom-cache-dir))
      bookmark-old-default-file bookmark-default-file
      bookmark-file             bookmark-default-file
      bookmark-sort-flag        t)

(when (modulep! :checkers spell)
  (setq spell-fu-directory
        (+mkdirp (expand-file-name "etc/spell-fu/" doom-cache-dir)))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary
                "en-personal"
                (expand-file-name "aspell.en.pws" doom-private-dir))))))

(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)

(use-package! smartparens
  :hook (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (pcase-dolist (`(,open . ,close) '(("(" . ")")
                                     ("[" . "]")
                                     ("{" . "}")))
    ;; remove all default rules
    (sp-pair open close :post-handlers nil :unless nil)
    ;; add sole exception
    (sp-pair open close :unless '(:add sp-in-string-p))))

(use-package! pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.05
        pulsar-iterations 5
        pulsar-face 'pulsar-yellow
        pulsar-highlight-face 'pulsar-magenta)
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

(when IS-MAC
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches   "-aBhl --group-directories-first")
  (map! :map dired-mode-map "r"  #'reveal-in-osx-finder))
(map! :map dired-mode-map "C-l" #'dired-up-directory)

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("p" "~/Desktop/" "Desktop")))
  :config
  (setq dirvish-use-header-line 'global
        delete-by-moving-to-trash t)
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (setq dirvish-attributes '(all-the-icons collapse file-size subtree-state vc-state))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso \
--group-directories-first --no-group")
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map            ; dirvish inherits `dired-mode-map'
   ("^"   . dirvish-history-last)
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("h"   . dirvish-history-jump)   ; remapped `describe-mode'
   ("N"   . dirvish-narrow)
   ("s"   . dirvish-quicksort)      ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)        ; remapped `dired-view-file'
   ("y"   . dirvish-yank-menu)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(when (modulep! :completion vertico)
  (use-package! vertico
    :demand t
    :bind
    (("M-."      . #'embark-act)
     ("C-x B"    . #'+vertico/switch-workspace-buffer)
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
    :config
    (setq consult-grep-args
          "grep --null --line-buffered --color=never --ignore-case \
--exclude-dir=.git --line-number -I -r .")
    :bind
    (("M-i"     . #'consult-imenu)
     ("C-c M-o" . #'consult-multi-occur)
     ("C-x b"   . #'consult-buffer)
     ("C-x 4 b" . #'consult-buffer-other-window)
     ("C-x 5 b" . #'consult-buffer-other-frame)
     ("C-x r b" . #'consult-bookmark)
     ("M-g g"   . #'consult-goto-line))))

(when (modulep! :completion company)
  (use-package! company
    :defer t
    :config
    (setq company-idle-delay 0.9)))

(setq magit-revision-show-gravatars t
      forge-database-file
      (expand-file-name "forge/forge-database.sqlite" doom-cache-dir))
(add-hook! 'magit-mode-hook (lambda () (magit-delta-mode +1)))

(use-package! mu4e
  :when (modulep! :email mu4e)
  :defer t
  :init
  ;; general emacs mail behavior
  (setq compose-mail-user-agent-warnings nil
        message-wide-reply-confirm-recipients t
        message-confirm-send nil
        message-kill-buffer-on-exit t
        message-directory "~/.mail")

  ;; sendmail configuration
  (setq message-mail-user-agent t
        mail-user-agent 'message-user-agent
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-send-mail-function #'smtpmail-send-it
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil t
        mail-envelope-from 'header
        mail-specify-envelope-from t)

  ;; general mu4e settings
  (setq mu4e-attachment-dir "~/Documents/"
        mu4e-change-filenames-when-moving t
        mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-update-interval (* 10 60))

  ;; mu4e appearance customizations
  (setq mu4e-headers-attach-mark    '("a" . " ")
        mu4e-headers-calendar-mark  '("c" . " ")
        mu4e-headers-draft-mark     '("D" . " ")
        mu4e-headers-encrypted-mark '("x" . " ")
        mu4e-headers-flagged-mark   '("F" . " ")
        mu4e-headers-list-mark      '("l" . " ")
        mu4e-headers-passed-mark    '("P" . " ")
        mu4e-headers-personal-mark  '("p" . " ")
        mu4e-headers-replied-mark   '("R" . " ")
        mu4e-headers-seen-mark      '("s" . " ")
        mu4e-headers-signed-mark    '("S" . " ")
        mu4e-headers-trashed-mark   '("T" . " ")
        mu4e-headers-unread-mark    '("u" . " "))
  :config
  ;; configure mu4e main screen bookmarks
  (setq mu4e-bookmarks
        '((:name "Personal inbox" :query "m:/personal/INBOX" :key ?p)
          (:name "Cognitect inbox" :query "m:/cognitect/INBOX" :key ?c)
          (:name "Legacy inbox" :query "m:/legacy/INBOX" :key ?x)

          (:name "re: Randi" :query "Randi" :key ?r)
          (:name "re: Sebastian" :query "Sebastian" :key ?s)
          (:name "from: Mom" :query "from:jennyromney" :key ?j)
          (:name "from: Dad" :query "from:leslieromney" :key ?l)
          (:name "from: Wes" :query "from:wesleyromney" :key ?y)

          (:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
          (:name "Messages with attachments" :query "mime:*" :key ?a)
          (:name "Messages with images" :query "mime:image/*" :key ?i)
          (:name "Flagged messages" :query "flag:flagged" :key ?f)))

  ;; let mu4e know these are gmail accounts
  (setq +mu4e-gmail-accounts
        '(("christian.a.romney@gmail.com"        . "/personal")
          ("christian.romney@thinkrelevance.com" . "/cognitect")
          ("xmlblog@gmail.com"                   . "/legacy")))

  ;; let mu4e know which account is which
  (set-email-account!
   "personal"
   '((user-mail-address      . "christian.a.romney@gmail.com")
     (smtpmail-smtp-user     . "christian.a.romney@gmail.com")
     (mu4e-sent-folder       . "/personal/[Gmail]/Sent Mail")
     (mu4e-drafts-folder     . "/personal/[Gmail]/Drafts")
     (mu4e-trash-folder      . "/personal/[Gmail]/Trash")
     (mu4e-refile-folder     . "/personal/[Gmail]/All Mail")
     (mu4e-compose-signature . "\n\nRegards,\nChristian"))
   t)

  (set-email-account!
   "cognitect"
   '((user-mail-address      . "christian.romney@thinkrelevance.com")
     (smtpmail-smtp-user     . "christian.romney@thinkrelevance.com")
     (mu4e-sent-folder       . "/cognitect/[Gmail]/Sent Mail")
     (mu4e-drafts-folder     . "/cognitect/[Gmail]/Drafts")
     (mu4e-trash-folder      . "/cognitect/[Gmail]/Trash")
     (mu4e-refile-folder     . "/cognitect/[Gmail]/All Mail")
     (mu4e-compose-signature . "\n\nRegards,\nChristian Romney"))
   t)

  (set-email-account!
   "legacy"
   '((user-mail-address      . "xmlblog@gmail.com")
     (smtpmail-smtp-user     . "xmlblog@gmail.com")
     (mu4e-sent-folder       . "/legacy/[Gmail]/Sent Mail")
     (mu4e-drafts-folder     . "/legacy/[Gmail]/Drafts")
     (mu4e-trash-folder      . "/legacy/[Gmail]/Trash")
     (mu4e-refile-folder     . "/legacy/[Gmail]/All Mail")
     (mu4e-compose-signature . "\n\nRegards,\nChristian"))
   t))

;; set up hooks and notifications
(after! mu4e
  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-notifications)
  (add-hook 'message-setup-hook #'message-sort-headers))

(message "=> loaded mail configuration")

(after! circe
  (require 'auth-source)
  (let* ((host "irc.libera.chat")
         (user (custom/read-auth-username :host host))
         (pass (custom/read-auth-password :host host)))
    (set-irc-server! host
                     `(:tls t
                       :port 6697 ;; TLS port
                       :nick ,user
                       :sasl-username ,user
                       :sasl-password ,pass
                       :channels ("#clojure" "#emacs")))))

(map! "C-e"       #'move-end-of-line
      "C-'"       #'avy-goto-line
      "C-:"       #'avy-goto-char
      "C-x \\"    #'align-regexp
      "C-x g"     #'magit-status
      "C-x P"     #'print-buffer
      "C-x r I"   #'string-insert-rectangle
      "C-x C-h"   #'add-file-local-variable-prop-line
      "C-x M-s"   #'transpose-sexps
      "C-x M-t"   #'transpose-paragraphs
      "C-c a"     #'org-agenda
      "C-c M-t"   #'transpose-sentences
      "M-/"       #'hippie-expand
      "M-o"       #'other-window
      "M-p"       #'fill-paragraph
      "M-%"       #'anzu-query-replace
      "C-c g"     #'google-this
      "M-\\"      #'custom/delete-horizontal-space
      "M-SPC"     #'custom/just-one-space
      "<s-right>" #'sp-forward-slurp-sexp
      "<s-left>"  #'sp-forward-barf-sexp
      "C-M-%"     #'anzu-query-replace-regexp
      "C-x t c"   #'display-temperature-at-point-conversions)

(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
(message "=> loaded global configuration")

(defun custom/org-rebuild-cache ()
  "Rebuild the `org-mode' (and `org-roam') cache(s)."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (when (modulep! :lang org +roam2)
    (org-roam-db-sync)
    (org-roam-update-org-id-locations)))

(defun custom/org-markup-word (theChar)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (+ 1 (region-end))))
        (save-excursion
          (goto-char beg)
          (insert-char theChar)

          (goto-char end)
          (insert-char theChar)))
    (save-excursion
      (backward-word)
      (insert-char theChar)
      (forward-word)
      (insert-char theChar)))
  (forward-char))

(defun custom/org-italicize-word ()
  (interactive)
  (custom/org-markup-word #x00002F))

(defun custom/org-bold-word ()
  (interactive)
  (custom/org-markup-word #x00002A))

(defun custom/org-code-word ()
  (interactive)
  (custom/org-markup-word #x00007E))

(defun custom/org-underline-word ()
  (interactive)
  (custom/org-markup-word #x00005F))

(defun custom/org-verbatim-word ()
  (interactive)
  (custom/org-markup-word #x00003D))

(defun custom/org-strike-word ()
  (interactive)
  (custom/org-markup-word #x00002B))

(defvar +info-dir "~/Documents/personal/notes"
  "The root for all notes, calendars, agendas, todos, attachments,
and bibliographies.")
(use-package! org
  :defer t
  :init
  (setq
   org-directory              (expand-file-name "content" +info-dir)
   org-roam-directory         (expand-file-name "roam" org-directory)
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
                         "#+title: %<%Y-%m-%d>\n"))))
  :config
  ;; behaviors
  (setq org-export-html-postamble          nil
        org-hide-emphasis-markers          t
        org-html-validation-link           nil
        org-log-done                       nil
        org-outline-path-complete-in-steps nil
        org-M-RET-may-split-line           t
        org-refile-use-cache               t
        org-refile-use-outline-path        t
        org-return-follows-link            t
        org-src-window-setup               'current-window
        org-use-fast-todo-selection        t
        org-use-sub-superscripts           "{}")

  ;; refiling
  (setq
   org-refile-targets
   '((nil :maxlevel . 5)
     (org-agenda-files :maxlevel . 5)))

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
          (:grouptags)
          ("cognicast"  . ?c)
          ("perf-cycle" . ?f)
          (:endgrouptag)
          ("personal"   . ?p)))

  ;; clock in/out
  (setq org-clock-persist-file
        (expand-file-name "etc/org-clock-save.el" doom-cache-dir))

  ;; capture
  (setq +org-capture-changelog-file "changelog.org.gpg"
        +org-capture-notes-file "notes.org.gpg"
        +org-capture-projects-file "projects.org.gpg"
        +org-capture-todo-file "todo.org.gpg"
        +org-capture-journal-file "journal.org.gpg"

        org-capture-templates
        `(("t" "Todo" entry (file+headline "todo.org.gpg" "Todos")
           "* TODO %^{Task} %^G")))
  (map!
   (:when (modulep! :lang org +roam2)
    :desc "Rebuild Roam cache" "C-c n r b" #'custom/org-rebuild-cache)
   (:map org-mode-map
    "C-. o b" #'custom/org-bold-word
    "C-. o c" #'custom/org-code-word
    "C-. o i" #'custom/org-italicize-word
    "C-. o s" #'custom/org-strike-word
    "C-. o u" #'custom/org-underline-word
    "C-. o v" #'custom/org-verbatim-word)))

(use-package! org-glossary
  :after org
  :hook (org-mode . org-glossary-mode)
  :init
  ;; this macro supplies theme color names inside the body
  (defface org-glossary-term
    '((default :inherit (popup-tip-face)
        :weight normal))
    "Base face used for term references.")
  :config
  (setq org-glossary-fontify-types-differently nil)
  (map!
   (:map org-mode-map
    "C-. o g" #'org-glossary-create-definition)))

(use-package! zotxt
  :after org
  :hook (org-mode . org-zotxt-mode)
  :config
  (setq bibtex-dialect                  'biblatex
        org-cite-csl-styles-dir         (expand-file-name "zotero/styles/" +info-dir)))

(when (modulep! :tools biblio)
  (setq! citar-bibliography
         (list (expand-file-name "references.bib" +info-dir))))

(use-package! org-agenda
  :defer t
  :config
  (setq org-agenda-file-regexp            "\\`[^.].*\\.org\\(\\.gpg\\)?\\'"
        org-agenda-files                  (list org-directory
                                                org-roam-directory
                                                org-roam-dailies-directory)
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
        org-icalendar-combined-agenda-file
        (expand-file-name "org.ics" org-directory)
        org-agenda-custom-commands
        ' (("d" "Dashboard"
            ((agenda "" ((org-agenda-span 10)))
             (tags-todo "+PRIORITY=\"A\"")
             (tags-todo "work")
             (tags-todo "personal")))
           ("n" "Agenda and all TODOs"
            ((agenda "" ((org-agenda-span 10)))
             (alltodo ""))))))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-priority t)
                                  (:auto-tags t)
                                  (:auto-todo t)))
  (org-super-agenda-mode))

(use-package! calendar
  :after org
  :config
  (defface +calendar-holiday
    '((t . (:inherit pulsar-cyan)))
    "Face for holidays in calendar.")

  (defface +calendar-today
    '((t . (:foreground "violet red" :box t)))
    "Face for the current day in calendar.")

  (defface +calendar-appointment
    '((t . (:inherit pulsar-yellow)))
    "Face for appointment diary entries in calendar.")

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
        (expand-file-name "appointment-diary.gpg" org-directory))
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package! holidays
  :after org
  :config
  (require 'brazilian-holidays)
  (setq calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays
                holiday-christian-holidays
                holiday-solar-holidays
                brazilian-holidays--general-holidays
                brazilian-holidays-sp-holidays)))

(use-package! graphviz-dot-mode
  :defer t
  :config
  (setq graphviz-dot-indent-width 2))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! org
  (when (modulep! :lang plantuml)
    (setq plantuml-default-exec-mode 'jar))

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

(after! org
  (setq org-re-reveal-center               t
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
        org-re-reveal-root                 "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.7.0/"
        org-re-reveal-title-slide          "%t"
        reveal_inter_presentation_links    t))

(setq org-ellipsis                       "…"
      org-fontify-done-headline          t
      org-fontify-emphasized-text        t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line     t
      org-modern-star                    '("◉" "○" "▣" "□" "◈" "◇" "✦" "✧" "✻" "✾")
      org-pretty-entities                t
      org-src-fontify-natively           t
      org-src-tab-acts-natively          t
      org-startup-folded                 nil
      org-startup-indented               t)

(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
(add-hook! 'org-mode-hook #'org-modern-mode)
(add-hook! 'org-mode-hook :append
  (lambda ()
    (setq left-margin-width 2
          right-margin-width 2)))

(message "=> loaded org configuration")

(use-package! projectile
  :defer t
  :config
  (+mkdirp (expand-file-name "projectile" doom-cache-dir))
  (setq
   projectile-cache-file
   (expand-file-name "projectile/projectile.cache" doom-cache-dir)
   projectile-known-projects-file
   (expand-file-name "projectile/projectile.projects" doom-cache-dir)))

(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "deps.edn"))

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

;; TODO: try moving these to the :hook ()
(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojure-mode-hook :append #'subword-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurec-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurex-mode-hook #'turn-on-smartparens-strict-mode)

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
      (let ((port (custom/read-file-as-string found-port-file)))
        (message "Connecting clojure socket REPL on ephemeral shadow port %s" port)
        (inf-clojure (cons "localhost" port))))

     ;; option 2: check default port
     ((custom/port-open-p default-socket-repl-port)
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

(when (modulep! :checkers syntax)
  (use-package! flycheck-clj-kondo
    :when (modulep! :checkers syntax)
    :after flycheck))

(message "=> loaded clojure configuration")

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
  (message "=> loaded scheme configuration"))

(when (modulep! :lang cc)
  (map! :map c-mode-base-map
        ;; disassembler (objdump)
        "C-c o a"    #'disaster)

  ;; disassembler
  (use-package! disaster
    :commands (disaster)
    :init
    (setq disaster-assembly-mode 'nasm-mode)
    :config
    ;; the default -M att argument doesn't work for me using
    ;; Apple clang version 12.0.5 (clang-1205.0.22.9)
    ;; Target: x86_64-apple-darwin20.4.0
    (setq disaster-objdump "objdump -d -Sl --no-show-raw-insn"))
  (message "=> loaded C configuration"))
