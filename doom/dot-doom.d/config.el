;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; ==============================================================================
;;                                   HELP
;; ==============================================================================
;; Here are some additional functions/macros that could help you configure Doom:
;;
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;
;; ==============================================================================
;;                              PERSONALIZATION
;; ==============================================================================
;;
(add-to-list 'default-frame-alist              '(fullscreen . maximized))
(setq-default doom-scratch-initial-major-mode  'emacs-lisp-mode)
(setq user-full-name                           "Christian Romney"
      user-mail-address                        "christian.romney@cognitect.com"
      calendar-location-name                   "Pembroke Pines, FL"
      calendar-longitude                       -80.3432341
      calendar-latitude                        26.0170038
      calendar-week-start-day                  1
      org-directory                            "~/Documents/notes/"
      doom-theme                               'doom-one-light
      display-line-numbers-type                t
      confirm-kill-emacs                       nil
      +default-want-RET-continue-comments      nil
      fancy-splash-image                       "~/Pictures/logos/cognitect.png"
      doom-font                                (font-spec :family "Iosevka Nerd Font Mono"
                                                          :weight 'regular
                                                          :size 20)
      doom-big-font                            (font-spec :family "Iosevka Nerd Font Mono"
                                                          :weight 'regular
                                                          :size 40))

;; jfdi
;; doom-protect-fallback-buffer-h
;; persp-kill-buffer-query-function
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; ==============================================================================
;;                              GLOBAL SETTINGS
;; ==============================================================================

(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook!    'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
(electric-pair-mode +1)

;; ==============================================================================
;;                             KEY BINDINGS
;; ==============================================================================
;;
;; Global
;;
(map! "C-e"     #'move-end-of-line
      "C-'"     #'avy-goto-line
      "C-:"     #'avy-goto-char
      "C-x \\"  #'align-regexp
      "C-x g"   #'magit-status
      "C-x P"   #'print-buffer
      "C-x r I" #'string-insert-rectangle
      "C-x C-h" #'add-file-local-variable-prop-line
      "C-x M-s" #'transpose-sexps
      "C-x M-t" #'transpose-paragraphs
      "C-c a"   #'org-agenda
      "C-c M-t" #'transpose-sentences
      "M-/"     #'hippie-expand
      "M-o"     #'other-window
      "M-p"     #'fill-paragraph
      "M-%"     #'anzu-query-replace
      "C-c g"   #'google-this
      "C-M-%"   #'anzu-query-replace-regexp)

;;
;; Clojure
;;
;;
;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs
      :map cider-mode-map
      "C-s-x"   #'rebl-eval-defun-at-point
      "C-x C-r" #'rebl-eval-last-sexp)

(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)

;;
;; Dired
;;
(map! :map dired-mode-map
      "C-l"     #'dired-up-directory) ;; make dired navigate like helm
;;
;; ==============================================================================
;;                        Package Customization
;; ==============================================================================
;;
;; Magit
;;
(add-hook 'magit-mode-hook #'magit-delta-mode)
(setq magit-revision-show-gravatars t)

;;
;; Helm
;;
(when (featurep! :completion helm)
  (map! "M-i"   #'helm-imenu)
  (map! :map helm-map "<tab>"   #'helm-execute-persistent-action)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line t
        helm-split-window-in-side-p nil
        helm-move-to-line-cycle-in-source nil
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-echo-input-in-header-line nil))

;; Ivy
(when (featurep! :completion ivy)
  (map! "M-i"     #'counsel-imenu
        "C-c M-o" #'occur))

;; global keys

(defun personal-just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun personal-delete-horizontal-space ()
  "Command to delete all whitespace."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

(map! "M-\\"      #'personal-delete-horizontal-space
      "M-SPC"     #'personal-just-one-space
      "<s-right>" #'sp-forward-slurp-sexp
      "<s-left>"  #'sp-forward-barf-sexp)

;; Language Server

(setq lsp-ui-doc-max-height 20
      lsp-ui-doc-max-width  75)

;;
;; Org
;;
;; agenda settings
(setq org-agenda-include-diary t
      org-agenda-files '("~/Documents/notes/")
      org-agenda-show-log t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-start-on-weekday 1
      org-agenda-todo-ignore-deadlines t
      org-agenda-todo-ignore-scheduled t
      org-agenda-use-tag-inheritance nil
      org-agenda-window-setup 'current-window)

;; appearance
(setq org-ellipsis "…"
      org-pretty-entities t
      org-startup-indented t
      org-startup-folded nil
      org-fontify-done-headline t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-fontify-emphasized-text t
      org-src-fontify-natively t
      org-src-tab-acts-natively t

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

(setq org-export-html-postamble nil
      org-hide-emphasis-markers t
      org-html-validation-link nil
      org-log-done nil
      org-outline-path-complete-in-steps nil
      org-refile-use-cache t
      org-refile-use-outline-path t
      org-return-follows-link t
      org-src-window-setup 'current-window
      org-use-fast-todo-selection t
      org-use-sub-superscripts "{}"
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
          (tags-todo "work|reify")
          (tags-todo "personal")))
        ("n" "Agenda and all TODOs"
         ((agenda "" ((org-agenda-span 10)))
          (alltodo "")))))
;;
;; org-reveal
;;
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

;; org-babel
;; if tangling gives an error about "pdf-info-process-assert-running"
;; re-compile pdf-tools with M-x pdf-tools-install 
(after! org
  (progn
    (pdf-loader-install)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (css . t)
       (dot . t)
       (emacs-lisp . t)
       (java . t)
       (js . t)
       (makefile . t)
       (plantuml . t)
       (prolog . t)
       (python . t)
       (R . t)
       (ruby . t)
       (scheme . t)
       (sed . t)
       (shell . t)
       (sql . t)))))

;; org-capture
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

(add-hook! 'org-mode-hook #'turn-on-org-pretty-table-mode)
(add-hook! 'org-mode-hook (lambda () (setq left-margin-width 2 right-margin-width 2)))

;; ==============================================================================
;;                      Emacs Behavioral Customizations
;; ==============================================================================
;;
;; I like repeated searches to remain in the middle of the screen so I don't have
;; to scan my monitor for the place where I've landed. I can always stare at the
;; center of the screen and find my search results.
;;
;; ==============================================================================

(defun private/after-jump ()
  "Centers vertically and flashes the current line."
  (interactive)
  (recenter)
  (+nav-flash/blink-cursor))

(add-hook! 'isearch-mode-end-hook #'private/after-jump)

(defadvice isearch-forward
    (after isearch-forward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-repeat-forward)

(defadvice isearch-backward
    (after isearch-backward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-backward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
    (private/after-jump))
(ad-activate 'isearch-repeat-backward)

;; Automatically added by Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242631" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#9aedfe" "#f9f9f9"])
 '(custom-safe-themes
   (quote
    ("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "2a749c20af891c16571527d07976bbcf2bf31819fa7d322942b73386019f4d58" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "34b3219ae11acd81b2bb7f3f360505019f17d7a486deb8bb9c1b6d13c6616d2e" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default)))
 '(fci-rule-color "#e2e4e5")
 '(jdee-db-active-breakpoint-face-colors (cons "#282a36" "#57c7ff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#282a36" "#5af78e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#282a36" "#848688"))
 '(objed-cursor-color "#ff5c57")
 '(pdf-view-midnight-colors (cons "#f9f9f9" "#282a36"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#9aedfe" "#f9f9f9"])
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#5af78e")
    (cons 40 "#8df793")
    (cons 60 "#c0f898")
    (cons 80 "#f3f99d")
    (cons 100 "#f7e38c")
    (cons 120 "#fbcd7c")
    (cons 140 "#ffb86c")
    (cons 160 "#ff9e88")
    (cons 180 "#ff84a4")
    (cons 200 "#ff6ac1")
    (cons 220 "#ff659d")
    (cons 240 "#ff607a")
    (cons 260 "#ff5c57")
    (cons 280 "#e06663")
    (cons 300 "#c1716f")
    (cons 320 "#a27b7b")
    (cons 340 "#e2e4e5")
    (cons 360 "#e2e4e5")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
