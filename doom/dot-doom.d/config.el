;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; ===============================================================================
;;                                     STARTUP
;; ===============================================================================

(add-to-list 'default-frame-alist              '(fullscreen . maximized))
(setq-default doom-scratch-initial-major-mode  'emacs-lisp-mode)

;; ===============================================================================
;;                                 PERSONALIZATION
;; ===============================================================================

(setq user-full-name                           "Christian Romney"
      user-mail-address                        "christian.a.romney@gmail.com"

      org-directory                            "~/Documents/notes/"
      calendar-location-name                   "Pembroke Pines, FL"
      calendar-longitude                       -80.3432341
      calendar-latitude                        26.0170038
      confirm-kill-emacs                       nil
      calendar-week-start-day                  1
      +default-want-RET-continue-comments      nil)

;; ===============================================================================
;;                                    APPEARANCE
;; ===============================================================================

(setq doom-theme                               'doom-one-light
      doom-font                                (font-spec :family "Iosevka Nerd Font Mono"
                                                     :weight 'regular
                                                     :size 20)
      doom-big-font                            (font-spec :family "Iosevka Nerd Font Mono"
                                                          :weight 'regular
                                                          :size 40)
      fancy-splash-image                       "~/Pictures/logos/cognitect.png"
      display-line-numbers-type                t)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; ===============================================================================
;;                                GLOBAL BEHAVIORS
;; ===============================================================================
;;
;; smartparens globally conflicts with many org-mode bindings
(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook!    'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
(electric-pair-mode +1)

;; Don't ask me when killing buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; ===============================================================================
;;                              GLOBAL KEY BINDINGS
;; ===============================================================================

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
      ;;"C-c g"   #'google-this
      "C-M-%"   #'anzu-query-replace-regexp)

;; ===============================================================================
;;                           DOOM PACKAGE CUSTOMIZATION
;; ===============================================================================

;; +-----------------------------------------------------------------------------+
;; |                                  Ivy                                        |
;; +-----------------------------------------------------------------------------+

(when (featurep! :completion ivy)
  (map! "M-i"     #'counsel-imenu
        "C-c M-o" #'occur
        "C-s"     #'swiper-isearch)

  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

;; +-----------------------------------------------------------------------------+
;; |                                  Magit                                      |
;; +-----------------------------------------------------------------------------+

(setq magit-revision-show-gravatars t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
