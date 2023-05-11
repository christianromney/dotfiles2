(message "> Beginning Emacs initialization.")
(setq user-full-name    "Christian Romney"
      user-mail-address "christian.a.romney@gmail.com")

;; reset themeing
(mapc #'disable-theme custom-enabled-themes)

(setq doom-font                   "JetBrains Mono:pixelsize=20"
      inhibit-startup-message     t
      display-line-numbers-type   t)

(use-package nano-theme
 :init
 (require 'nano-theme)
 (setq-default tab-width 2)
 (setq-default cursor-type 'bar)
 (setq nano-fonts-use nil)
 (setq nano-font-family-monospaced "JetBrains Mono")
 (setq nano-font-size 20)

 ;; Default frame settings
 (setq default-frame-alist
       (append (list
                '(min-height . 1)  '(height . 45)
                '(min-width  . 1)  '(width  . 81)
                '(vertical-scroll-bars . nil)
                '(internal-border-width . 24)
                '(left-fringe . 0)
                '(right-fringe . 0)
                '(undecorated-round . t) ;; emacs-plus@29 only
                '(tool-bar-lines . 0)
                '(menu-bar-lines . 0)
                '(fullscreen . maximized)
                )))
 (load-theme 'doom-one-light t)
 (nano-light))

;; double rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(setq face-remapping-alist
'((show-paren-match . (:inherit pulsar-yellow))      ;; yellow highlight
  (show-paren-mismatch . (:inherit flycheck-error))  ;; red squiggly underline
  ))

(setq +default-want-RET-continue-comments nil
      +file-templates-dir                 (expand-file-name "etc/snippets" doom-private-dir)
      doom-cache-dir                      user-emacs-directory)

(add-to-list 'doom-large-file-size-alist
             '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

(setq +doom-dashboard-banner-dir
      (expand-file-name "etc/banners" doom-private-dir))

(setq +doom-dashboard-banner-file
      (expand-file-name "splash.png" +doom-dashboard-banner-dir))

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

(defun custom/keychain-api-token-for-host (host)
  "Reads the keychain internet password for the given host."
  (string-trim
   (shell-command-to-string
    (string-join `("security find-internet-password -s " ,host " -w") ""))))

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

(defun custom/work-computer-p ()
  "Uses a heuristic to determine whether this is my work laptop."
  (string-equal
     (string-trim (shell-command-to-string "hostname"))
     (string-trim (shell-command-to-string "whoami"))))

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
      use-short-answers           t
      enable-dir-local-variables  t
      enable-local-variables      t
      initial-major-mode          'lisp-interaction-mode
      user-emacs-directory        doom-cache-dir
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
(setq native-comp-async-report-warnings-errors 'silent)

(setq abbrev-file-name (expand-file-name  "etc/abbrev_defs" doom-private-dir)
      save-abbrevs     'silent)
(setq-default abbrev-mode t)

(setq bookmark-default-file     (expand-file-name "etc/bookmarks" doom-private-dir)
      bookmark-old-default-file bookmark-default-file
      bookmark-file             bookmark-default-file
      bookmark-sort-flag        t)

(when (modulep! :checkers spell)
  (setq spell-fu-directory
        (+mkdirp (expand-file-name "etc/spell-fu/" doom-private-dir)))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary
                "en-personal"
                (expand-file-name "aspell.en.pws" spell-fu-directory))))))

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
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)))

(when (modulep! :completion company)
  (use-package! company
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
                       :channels ("#clojure" "#datomic" "#emacs")))))

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

(defun custom/markup-word (markup-char)
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
    (progn
      (save-excursion
        (replace-region-contents
         start end
         (lambda ()
           (s-wrap text
                   (char-to-string markup-char)
                   (char-to-string markup-char))))))))

(defun custom/org-italicize-word ()
  (interactive)
  (custom/markup-word #x00002F))

(defun custom/org-bold-word ()
  (interactive)
  (custom/markup-word #x00002A))

(defun custom/org-code-word ()
  (interactive)
  (custom/markup-word #x00007E))

(defun custom/org-underline-word ()
  (interactive)
  (custom/markup-word #x00005F))

(defun custom/org-verbatim-word ()
  (interactive)
  (custom/markup-word #x00003D))

(defun custom/org-strike-word ()
  (interactive)
  (custom/markup-word #x00002B))

(defvar +info-dir "~/Documents/personal/notes"
  "The root for all notes, calendars, agendas, todos, attachments,
and bibliographies.")
(use-package! org
  :defer t
  :init
  (setq org-directory (expand-file-name "content" +info-dir)
        org-modules   '(ol-bibtex ol-bookmark ol-docview
                        ol-doi org-checklist org-id
                        org-tempo))

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
  (setq org-refile-use-cache                   t ;; use C-0 C-c C-w to clear cache
        org-refile-use-outline-path            t
        org-refile-allow-creating-parent-nodes t
        org-refile-targets                     '((nil :maxlevel . 5)
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
          ("personal"   . ?m)
          ("FLAGGED"    . ?f)))

  ;; clock in/out
  (setq org-clock-persist-file
        (expand-file-name "etc/org-clock-save.el" doom-cache-dir))

  ;; capture
  (setq +org-capture-changelog-file "changelog.org"
        +org-capture-notes-file     "notes.org"
        +org-capture-projects-file  "projects.org"
        +org-capture-todo-file      "todo.org"
        +org-capture-journal-file   "journal.org"
        org-capture-templates
        `(("t" "Todo" entry (file+headline "todo.org" "Todos")
           "* TODO %^{Task} %^G")))
  (map!
   (:map org-mode-map
    :desc "org markup"
    :prefix ("C-, o" . "org markup word")
    :desc "bold"          "b" #'custom/org-bold-word
    :desc "code"          "c" #'custom/org-code-word
    :desc "italics"       "i" #'custom/org-italicize-word
    :desc "strikethrough" "s" #'custom/org-strike-word
    :desc "underline"     "u" #'custom/org-underline-word
    :desc "verbatim"      "v" #'custom/org-verbatim-word)))

(use-package! consult-org-roam
   :defer t
   :after org
   :init
   (require 'consult-org-roam)
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   ;; Define some convenient keybindings as an addition
   :bind
   ("C-c n r o f" . consult-org-roam-file-find)
   ("C-c n r o b" . consult-org-roam-backlinks)
   ("C-c n r o l" . consult-org-roam-forward-links)
   ("C-c n r o s" . consult-org-roam-search)
   ("C-c n r c"   . custom/org-rebuild-cache))

;; disabled
(when nil
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
                                        (svg-tag-make tag :face 'org-modern-tag
                                                      :padding 1 :radius 3))))
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
                                       :face 'pulsar-yellow
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
                              :end -1 :inverse t :crop-left t :margin 0))))))))

(use-package! org-glossary
  :after org
  :hook (org-mode . org-glossary-mode)
  :init
  ;; this macro supplies theme color names inside the body
  (defface org-glossary-term
    '((default :inherit (show-paren-match)
        :background "antique white"
        :weight normal))
    "Base face used for term references.")
  :config
  (setq org-glossary-fontify-types-differently nil)
  (map!
   (:map org-mode-map
    "C-| o g" #'org-glossary-create-definition
    "C-| o G" #'org-glossary-goto-term-definition)))

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
        org-re-reveal-root
        "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.5.0/reveal.js"
        org-re-reveal-title-slide          "%t"
        reveal_inter_presentation_links    t))

(face-spec-set 'org-modern-tag
               '((default :weight normal :background "#d1bce5")))

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

(use-package! openai
  :init
  (setq openai-key (custom/keychain-api-token-for-host "api.openai.com"))
  (when (and (custom/port-open-p 3005)
             (custom/work-computer-p))
    (setq openai-base-url "http://0.0.0.0:3005/v1")))

(message "=> loaded openai package")

(use-package! gptel
  :after openai
  :commands (gptel)
  :init
  (map! :desc "ChatGPT" "C-c M-h c" #'gptel)
  :config
  (setq gptel-api-key openai-key
        gptel-model "gpt-3.5-turbo")
  (when (and (custom/port-open-p 3005)
             (custom/work-computer-p))
    (setq gptel-openai-endpoint "http://0.0.0.0:3005/v1"
          gptel-stream nil)))

(use-package! codegpt
  :after openai
  :commands (codegpt)
  :init
  (map!
   :prefix ("C-c M-h o" . "coding assistant")
   :desc "CodeGPT"        "g" #'codegpt
   :desc "Document code"  "d" #'codegpt-doc
   :desc "Explain code"   "e" #'codegpt-explain
   :desc "Fix code"       "f" #'codegpt-fix
   :desc "Improve code"   "i" #'codegpt-improve)
  :config
  (setq codegpt-tunnel 'chat
        codegpt-model "gpt-3.5-turbo"))
(message "=> loaded CodeGPT")

(use-package! dall-e
  :after openai
  :commands (dall-e)
  :init
  (map! :desc "Dall-E" "C-c M-h d" #'dall-e)
  :config
  (setq dall-e-n 3
        dall-e-size "256x256"
        dall-e-display-width 200
        dall-e-cache-dir (expand-file-name "dall-e" doom-cache-dir)))
(message "=> loaded Dall-E")

(use-package! org-ai
  :commands (org-ai-mode org-ai-global-mode)
  :after (org openai)
  :hook (org-mode . org-ai-mode)
  :init
  (add-to-list 'org-structure-template-alist '("ai" . "ai"))
  (org-ai-global-mode)
  :config
  (org-ai-install-yasnippets)
  (setq org-ai-openai-api-token (custom/keychain-api-token-for-host "api.openai.com"))
  (when (and (custom/port-open-p 3005)
             (custom/work-computer-p))
    (setq org-ai-openai-chat-endpoint "http://0.0.0.0:3005/v1/chat/completions"
          org-ai-openai-completion-endpoint "http://0.0.0.0:3005/v1/completions"
          org-ai-on-project-use-stream nil)))
(message "=> loaded org-ai")

(message "> Emacs initialization complete.")
