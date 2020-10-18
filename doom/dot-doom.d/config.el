;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; ===============================================================================
;;                                     STARTUP
;; ===============================================================================

(add-to-list 'default-frame-alist              '(fullscreen . maximized))

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
      fancy-splash-image                       (concat doom-private-dir "cognitect.png")
      display-line-numbers-type                t)

;; ===============================================================================
;;                                GLOBAL BEHAVIORS
;; ===============================================================================

;; smartparens globally conflicts with many org-mode bindings
(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook!    'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
(electric-pair-mode +1)

;; Don't ask me when killing process buffers
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode
      kill-buffer-query-functions     (remq 'process-kill-buffer-query-function
                                            kill-buffer-query-functions))

;; I like repeated searches to remain in the middle of the screen so I don't
;; have to scan my monitor for the place where I've landed. I can always stare
;; at the center of the screen and find my search results.

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

;; ===============================================================================
;;                              GLOBAL KEY BINDINGS
;; ===============================================================================

(defun +personal/just-one-space ()
  "Command to delete all but one whitespace character."
  (interactive)
  (just-one-space -1))

(defun +personal/delete-horizontal-space ()
  "Command to delete all whitespace."
  (interactive)
  (just-one-space -1)
  (sp-backward-delete-char))

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
      "M-\\"      #'+personal/delete-horizontal-space
      "M-SPC"     #'+personal/just-one-space
      "<s-right>" #'sp-forward-slurp-sexp
      "<s-left>"  #'sp-forward-barf-sexp
      "C-M-%"     #'anzu-query-replace-regexp)

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

  (after! ivy
    (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))))

;; +-----------------------------------------------------------------------------+
;; |                                  Magit                                      |
;; +-----------------------------------------------------------------------------+

(setq magit-revision-show-gravatars t)

;; Temporarily disable buggy magit-delta-mode
;; (add-hook! 'magit-mode-hook #'magit-delta-mode)

;; ===============================================================================
;;                             MODE CUSTOMIZATIONS
;; ===============================================================================

(load! "+org")
(load! "+clojure")
