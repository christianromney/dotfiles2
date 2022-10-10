(setq user-full-name    "Christian Romney"
      user-mail-address "christian.a.romney@gmail.com")

(setq doom-font                   "JetBrains Mono:pixelsize=20"
      inhibit-startup-message     t
      display-line-numbers-type   t
      fancy-splash-image          (concat doom-private-dir "splash.png"))

(use-package! doom-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (load-theme 'doom-ayu-light t)

  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-ayu-light")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; tab width
(setq-default tab-width 2)

;; ligature support
(mac-auto-operator-composition-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; double rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(setq face-remapping-alist
'((show-paren-match . (:inherit pulsar-yellow))      ;; yellow highlight
  (show-paren-mismatch . (:inherit flycheck-error))) ;; red squiggly underline
)

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
(setq native-comp-async-report-warnings-errors 'silent)

(setq +default-want-RET-continue-comments nil
      +file-templates-dir                 (expand-file-name "snippets" doom-private-dir)
      doom-cache-dir                      user-emacs-directory)

(add-to-list 'doom-large-file-size-alist
             '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

(require 'smtpmail)
(setq smtpmail-queue-mail nil)

(require 'sendmail)
(setq sendmail-program (executable-find "msmtp")
      send-mail-function #'smtpmail-send-it)

(require 'message)
(setq compose-mail-user-agent-warnings nil
      mail-envelope-from 'header
      mail-specify-envelope-from t
      mail-user-agent 'message-user-agent
      message-confirm-send nil
      message-directory "~/.mail"
      message-kill-buffer-on-exit t
      message-mail-user-agent t
      message-sendmail-envelope-from 'header
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t
      message-wide-reply-confirm-recipients t
      message-send-mail-function #'smtpmail-send-it)

(use-package! notmuch
  :bind (("C-c m m" . #'notmuch-hello)
         ("C-c m u" . #'+notmuch/update)
         ("C-c m c" . #'+notmuch/compose))
  :config
  (setq +notmuch-sync-backend             'mbsync
        notmuch-hello-auto-refresh        t
        notmuch-hello-recent-searches-max 20
        notmuch-message-headers           '("To" "Cc" "Subject" "Date")
        notmuch-message-headers-visible   t
        notmuch-search-oldest-first       nil
        notmuch-show-all-tags-list        t
        notmuch-show-logo                 nil
        notmuch-unthreaded-show-out       nil
        notmuch-wash-wrap-lines-length    120)

  (setq notmuch-saved-searches
        '((:name "unread (inbox)"
           :query "tag:unread and tag:inbox"
           :key "u"
           :sort-order newest-first)

          (:name "unread (all)"
           :query "tag:unread not tag:archived"
           :key "U"
           :sort-order newest-first)

          (:name "inbox"
           :query "tag:inbox"
           :key "i"
           :sort-order newest-first)

          (:name "flagged"
           :query "tag:flagged"
           :key "f"
           :sort-order newest-first)

          (:name "all mail"
           :query "*"
           :key "a"
           :sort-order newest-first)

          (:name "attachments"
           :query "tag:attachment"
           :key "A"
           :sort-order newest-first)

          (:name "sent"
           :query "tag:sent"
           :key "t"
           :sort-order newest-first)

          (:name "todo"
           :query "tag:todo not tag:archived"
           :key "T"
           :sort-order newest-first)

          (:name "drafts"
           :query "tag:draft not tag:archived"
           :key "d"
           :sort-order newest-first)

          (:name "mailing lists"
           :query "tag:lists not tag:archived"
           :key "M"
           :sort-order newest-first)

          (:name "lists/clojure"
           :query "(tag:lists/clojure or tag:lists/clojure-dev) not tag:archived"
           :key "C"
           :sort-order newest-first)

          (:name "lists/datomic"
           :query "tag:lists/datomic not tag:archived"
           :key "D"
           :sort-order newest-first)))
  ;; tags
  (setq notmuch-archive-tags           '("-inbox" "+archived")
        notmuch-message-replied-tags   '("+replied")
        notmuch-message-forwarded-tags '("+forwarded")
        notmuch-show-mark-read-tags    '("-unread")
        notmuch-draft-tags             '("+draft")
        notmuch-draft-folder           "drafts"
        notmuch-draft-save-plaintext   'ask)

  (setq notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flag"   (propertize tag 'face 'notmuch-tag-flagged))))

  (setq notmuch-tag-deleted-formats
        '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
          (".*"     (notmuch-apply-face tag `notmuch-tag-deleted))))

  ;; Writing email
  (setq notmuch-address-command 'internal
        notmuch-always-prompt-for-sender t
        notmuch-crypto-get-keys-asynchronously t
        notmuch-crypto-process-mime t
        notmuch-maildir-use-notmuch-insert t
        notmuch-mua-cite-function 'message-cite-original-without-signature
        notmuch-mua-compose-in 'current-window
        notmuch-mua-hidden-headers nil
        notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never
        notmuch-mua-user-agent-function #'notmuch-mua-user-agent-full)

  ;; Reading email
  (setq notmuch-show-relative-dates                  t
        notmuch-show-all-multipart/alternative-parts nil
        notmuch-show-indent-messages-width           0
        notmuch-show-indent-multipart                nil
        notmuch-show-part-button-default-action
        'notmuch-show-view-part)
  (message "-> done loading notmuch config."))

(after! notmuch
  (set-popup-rule! "^\\*notmuch-hello" :ignore t))

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
         :when (modulep! :email mu4e)
         :action =mu4e)
        ("Open mail"
         :icon (all-the-icons-faicon "inbox" :face 'doom-dashboard-menu-title)
         :when (modulep! :email notmuch)
         :action notmuch-hello)
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
  :hook (dired-mode . diredfl-mode))

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
  (setq org-directory              (expand-file-name "content" +info-dir))

  ;;
  (setq
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
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  :config
  (setq org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
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

  ;; refiling
  (setq org-refile-use-cache               t
        org-refile-use-outline-path        t
           org-refile-targets
        '((nil :maxlevel . 5)
          (org-agenda-files :maxlevel . 5)))
  ;; todo
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
          (:grouptags)
          ("cognicast"  . ?c)
          ("perf-cycle" . ?p)
          (:endgrouptag)
          ("personal"   . ?m)
          ("FLAGGED"    . ?f)))

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

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                    nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

(use-package! svg-tag-mode
  :hook ((org-agenda-mode org-super-agenda-mode). svg-tag-mode)
  :config
  (setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag)
                                    (svg-tag-make tag :face 'org-modern-tag :radius 8))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :radius 8 :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))

        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))

        ;; Task Statuses
        ("TODO" . ((lambda (tag)
                     (svg-tag-make "TODO" :face 'org-modern-todo
                                   :inverse t :margin 0 :radius 8))))
        ("WIP" . ((lambda (tag)
                    (svg-tag-make "WIP"
                                  :face 'org-modern-todo
                                  :inverse nil :margin 0 :radius 8))))
        ("WAIT" . ((lambda (tag)
                     (svg-tag-make "WAIT"
                                   :face 'pulsar-yellow
                                   :inverse nil :margin 0 :radius 8))))
        ("HOLD" . ((lambda (tag)
                     (svg-tag-make "HOLD"
                                   :face 'pulsar-yello
                                   :inverse t :margin 0 :radius 8))))
        ("DONE" . ((lambda (tag)
                     (svg-tag-make "DONE"
                                   :face 'org-modern-done
                                   :inverse nil :margin 0 :radius 8))))

        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                   (svg-tag-make tag
                                                                 :end -1
                                                                 :crop-left t))))


        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-date-active
                          :inverse t :beg 1 :end -1 :margin 0))))

        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-date-active
                          :beg 1 :inverse t :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-time-active
                          :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
        (,(format "\\(\\[%s\\]\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-date-inactive
                          :beg 1 :end -1 :inverse t :margin 0 :face))))
        (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-date-inactive
                          :beg 1 :inverse t :crop-right t :margin 0))))
        (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag
                          :face 'org-modern-time-inactive
                          :end -1 :inverse t :crop-left t :margin 0)))))))

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
  (defface org-glossary-term
    '((default :inherit (popup-tip-face)
        :weight normal))
    "Base face used for term references.")
  (face-spec-set 'org-agenda-date
                 '((default :weight normal)))
  (face-spec-set 'org-agenda-date-weekend
                 '((default :foreground "#399ee6" :weight normal)))
  (face-spec-set 'org-agenda-diary
                 '((default :weight normal :foreground "#86b300")))
  (face-spec-set 'org-agenda-date-today
                 '((default :foreground "#f07171" :slant italic :weight normal)))

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
        org-agenda-start-on-weekday       1
        org-agenda-todo-ignore-deadlines  t
        org-agenda-todo-ignore-scheduled  t
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
        (expand-file-name "appointment-diary" org-directory))
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

(use-package paren
  :config
  (setq blink-matching-paren t
      show-paren-mode t
      show-paren-style 'parenthesis
      show-paren-delay 0))

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

(message "=> loaded all configuration.")
