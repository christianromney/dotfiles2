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
      confirm-kill-emacs                       nil
      +default-want-RET-continue-comments      nil
      enable-dir-local-variables               t
      enable-local-variables                   t)

;; ===============================================================================
;;                                    APPEARANCE
;; ===============================================================================
;; valid font-spec :weight values:
;; 'ultra-bold
;; 'extra-bold
;; 'bold
;; 'semi-bold
;; 'normal
;; 'semi-light
;; 'light
;; 'extra-light
;; 'ultra-light
(setq doom-theme                     'leuven
      doom-font                      (font-spec :family "Iosevka" :weight 'normal :size 20)
      fancy-splash-image             (concat doom-private-dir "splash.png")
      display-line-numbers-type      t)

(add-hook! 'rainbow-mode-hook
  (hl-line-mode (if rainbow-mode -1 +1)))

(setq-default tab-width 2)

;; load my custom functions before all other config
(load! "+custom-functions")

;; ===============================================================================
;;                                GLOBAL BEHAVIORS
;; ===============================================================================

;; move cache to private dir
(setq doom-cache-dir
      (custom/ensure-directory (expand-file-name ".local/cache/" doom-private-dir)))

;; Don't ask me when killing process buffers
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode
      kill-buffer-query-functions     (remq 'process-kill-buffer-query-function
                                            kill-buffer-query-functions))
;; Abbrev mode
(setq abbrev-file-name             ;; tell emacs where to read abbrev
        "~/.doom.d/abbrev_defs")
(setq save-abbrevs 'silent)
(setq-default abbrev-mode t)

;; spelling
(when (featurep! :checkers spell)
  (setq spell-fu-directory
        (custom/ensure-directory (expand-file-name "etc/spell-fu/" doom-cache-dir)))
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "en-personal"
                                                 (expand-file-name "aspell.en.pws" doom-private-dir)))
              (message "spelling dictionaries: %s" spell-fu-dictionaries))))

;; handy temperature conversion display in the echo area
(global-set-key (kbd "C-x t c") #'display-temperature-at-point-conversions)

;; -------------------------------------------------------------------------------
;;                                SMART(ER)PARENS
;; -------------------------------------------------------------------------------
;;
;; smartparens globally conflicts with many org-mode bindings
(remove-hook! 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook!    'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)

(use-package! smartparens
  :config
  (pcase-dolist (`(,open . ,close) '(("(" . ")")
                                     ("[" . "]")
                                     ("{" . "}")))
    ;; remove all default rules
    (sp-pair open close :post-handlers nil :unless nil)
    ;; add sole exception
    (sp-pair open close :unless '(:add sp-in-string-p))))

;; -------------------------------------------------------------------------------
;;                             NAVIGATION BEHAVIOR
;; -------------------------------------------------------------------------------
;;
;; I like repeated searches to remain in the middle of the screen so I don't
;; have to scan my monitor for the place where I've landed. I can always stare
;; at the center of the screen and find my search results.
;;
(add-hook! 'isearch-mode-end-hook
           #'custom/flash-view-centered)
(defadvice isearch-forward
    (after isearch-forward-recenter activate)
  (custom/flash-view-centered))
(ad-activate 'isearch-forward)

(defadvice isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  (custom/flash-view-centered))
(ad-activate 'isearch-repeat-forward)

(defadvice isearch-backward
    (after isearch-backward-recenter activate)
    (custom/flash-view-centered))
(ad-activate 'isearch-backward)

(defadvice isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  (custom/flash-view-centered))
(ad-activate 'isearch-repeat-backward)

;; ===============================================================================
;;                              GLOBAL KEY BINDINGS
;; ===============================================================================

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
      "C-M-%"     #'anzu-query-replace-regexp)

;; ===============================================================================
;;                           PACKAGE CUSTOMIZATION
;; ===============================================================================

(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; +-----------------------------------------------------------------------------+
;; |                                   DIRED                                     |
;; +-----------------------------------------------------------------------------+
(setq insert-directory-program "/usr/local/bin/gls"
      dired-listing-switches   "-aBhl --group-directories-first")

(map! :map dired-mode-map
      "C-l" #'dired-up-directory
      "r"   #'reveal-in-osx-finder)

;; +-----------------------------------------------------------------------------+
;; |                                  Vertico                                    |
;; +-----------------------------------------------------------------------------+

(when (featurep! :completion vertico)
  ;; with Doom we can assume orderless, marginalia, consult, embark
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
          
          marginalia-align          'right))

  (use-package! consult
    :config
    (setq consult-grep-args
          "grep --null --line-buffered --color=never --ignore-case \
--exclude-dir=.git --line-number -I -r .")
    :bind
    (("M-i"      . #'consult-imenu)
     ("C-c M-o"  . #'consult-multi-occur)
     ("C-x b"    . #'consult-buffer)
     ("C-x 4 b"  . #'consult-buffer-other-window)
     ("C-x 5 b"  . #'consult-buffer-other-frame)
     ("C-x r b"  . #'consult-bookmark)
     ("M-g g"    . #'consult-goto-line)
     )))

(when (and (featurep! :completion company))
  (use-package! company
    :config
    (setq company-idle-delay 0.9)))

;; +-----------------------------------------------------------------------------+
;; |                                  Magit                                      |
;; +-----------------------------------------------------------------------------+

(setq magit-revision-show-gravatars t)
(add-hook! 'magit-mode-hook
  (lambda () (magit-delta-mode +1)))

;; +-----------------------------------------------------------------------------+
;; |                                  IRC                                        |
;; +-----------------------------------------------------------------------------+

(after! circe
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

;; ===============================================================================
;;                   LOAD ADDITIONAL MODE-SPECIFIC CUSTOMIZATIONS
;; ===============================================================================

(load! "+clojure")
(load! "+org")
(load! "+cc")
(load! "+scheme")
