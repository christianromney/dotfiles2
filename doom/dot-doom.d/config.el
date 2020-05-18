;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name                      "Christian Romney"
      user-mail-address                   "christian.a.romney@gmail.com"
      ;; agenda / calendar
      calendar-location-name              "Pembroke Pines, FL"
      calendar-longitude                  -80.34110799999999
      calendar-latitude                   26.017
      calendar-week-start-day             1
      org-directory                       "~/Dropbox/org/"
      org-agenda-files                    '("~/Dropbox/org/agenda/")
      ;; ui appearance / theme
      doom-theme                          'doom-snazzy
      doom-font                           (font-spec :family "Hack Nerd Font Mono" :size 18)
      display-line-numbers-type           t
      ;;password-cache-expiry               (*4 60 60) ; 4 hours
      )

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

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

(load! "smartparens.el")
(load! "key-bindings.el")
(load! "org-extras.el")
(load! "behavior.el")

(after! magit-mode
  (progn
    (magit-delta-mode +1)))

;; (defun romney/save-place-reposition ()
;;   "Force windows to recenter current line (with saved position)."
;;   (run-with-timer 0 nil
;;                   (lambda (buf)
;;                     (when (buffer-live-p buf)
;;                       (dolist (win (get-buffer-window-list buf nil t))
;;                         (with-selected-window win (recenter)))))
;;                   (current-buffer)))

;; (use-package saveplace ;; remember location when saving files
;;   :ensure nil
;;   :config
;;   (setq save-place-file (expand-file-name "saved-places" personal-data-dir))
;;   (save-place-mode)
;;   (add-hook 'find-file-hook 'romney/save-place-reposition t))



