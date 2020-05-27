;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personalization
;; ===============
(setq user-full-name                      "Christian Romney"
      user-mail-address                   "christian.a.romney@gmail.com"
      calendar-location-name              "Pembroke Pines, FL"
      calendar-longitude                  -80.34110799999999
      calendar-latitude                   26.017
      calendar-week-start-day             1
      ;; ui appearance / theme
      doom-theme                          'doom-snazzy
      doom-font                           (font-spec :family "Iosevka SS02" :weight 'medium :size 20)
      display-line-numbers-type           t)

;; Always maximize Emacs on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Here are some additional functions/macros that could help you configure Doom:
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

;; Key bindings
;; ============

;; = global =
(map! "C-x \\"  #'align-regexp
      "C-x g"   #'magit-status
      "M-/"     #'hippie-expand
      "C-x \\"  #'align-regexp
      "M-o"     #'other-window
      "C-'"     #'avy-goto-line
      "C-:"     #'avy-goto-char
      "C-C M-o" #'occur
      "C-x r I" #'string-insert-rectangle
      "C-x P"   #'print-buffer
      "M-p"     #'fill-paragraph
      "C-x C-h" #'add-file-local-variable-prop-line
      "C-x M-s" #'transpose-sexps
      "C-c M-t" #'transpose-sentences
      "C-x M-t" #'transpose-paragraphs
      "C-c a"   #'org-agenda
      "M-%"     #'anzu-query-replace
      "C-M-%"   #'anzu-query-replace-regexp
      "<f5>"    #'deadgrep)

;; = clojure mode =
(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs)

;; = ivy =
(when (featurep! :completion ivy)
  (map! :map ivy-minibuffer-map
        "C-m"     #'ivy-alt-done
        "C-l"     #'counsel-up-directory))

;; = helm =
(when (featurep! :completion helm)
  (map! :map helm-map
        "<tab>"   #'helm-execute-persistent-action))

;; = dired =
(map! :map dired-mode-map
      "C-l"     #'dired-up-directory)

;; Package customization
;; =====================

;; = magit =
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'magit-mode-hook #'magit-delta-mode)

;; = helm =
(setq helm-autoresize-max-height 0
      helm-autoresize-min-height 40
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-display-header-line t
      helm-split-window-in-side-p nil
      helm-move-to-line-cycle-in-source nil
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-echo-input-in-header-line nil)

;; = smartparens =
(after! smartparens
  (progn
    (defun personal-just-one-space ()
      "Command to delete all but one whitespace character."
      (interactive)
      (just-one-space -1))

    (defun personal-delete-horizontal-space ()
      "Command to delete all whitespace."
      (interactive)
      (just-one-space -1)
      (sp-backward-delete-char))

    (map! :map smartparens-mode-map
          "M-\\"    #'personal-delete-horizontal-space
          "M-SPC"   #'personal-just-one-space)

    ;; undo Doom's mistakes
    (sp-pair "'" nil :unless nil)
    (sp-pair "\"" nil :unless nil)
    (dolist (brace '("(" "{" "["))
      (sp-pair brace nil
               :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
               :unless nil))
    ;; (smartparens-global-strict-mode +1)
    ))

(add-hook! prog-mode (smartparens-strict-mode +1))

;; = org =
(setq org-agenda-include-diary t
      org-agenda-files '("~/Dropbox/org/agenda/")
      org-agenda-show-log t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-start-on-weekday 1
      org-agenda-todo-ignore-deadlines t
      org-agenda-todo-ignore-scheduled t
      org-agenda-use-tag-inheritance nil
      org-agenda-window-setup 'current-window
      org-directory "~/Dropbox/org/"
      org-ellipsis "…"
      org-export-html-postamble nil
      org-fontify-done-headline t
      org-hide-emphasis-markers t
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

      org-refile-targets
      '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))

      org-tag-alist
      '(("work"       . ?w)
        ("personal"   . ?p)
        ("study"      . ?s))

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
        (67 :foreground "#0098dd"))

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

;; = org-reveal =
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

;; smartparens bindings interfere with item (pro|de)motion in org-mode
;; (add-hook! org-mode
;;   (progn
;;     (turn-off-smartparens-mode)
;;     (turn-off-smartparens-strict-mode)))

;; = org-capture =
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

;; Emacs behavioral changes
;; ========================
(defun private/after-jump ()
  (recenter-top-bottom)
  (+nav-flash/blink-cursor))

;; Always center when jumping to search results
(add-hook 'isearch-mode-end-hook 'private/after-jump)
(add-hook 'imenu-after-jump-hook 'private/after-jump)

(defadvice isearch-forward
    (after isearch-forward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-repeat-forward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (private/after-jump))

(ad-activate 'isearch-repeat-backward)
