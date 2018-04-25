;;; romney-settings.el --- Set editor default configuration variables

;;; Commentary:
;;

;;; Code:
(setq user-full-name                      "Christian Romney"
      user-email-address                  "cromney@pointslope.com"
      calendar-location-name              "Pembroke Pines, FL"
      calendar-longitude                  -80.34110799999999
      calendar-latitude                   26.017
      custom-file                         "~/.emacs.d/custom.el"
      delete-old-versions                 t
      inhibit-startup-message             t                         ;; no initial message in scratch buffer
      auto-window-vscroll                 nil
      scroll-margin                       0                         ;; scroll settings
      scroll-conservatively               100000
      scroll-preserve-screen-position     1
      load-prefer-newer                   t                         ;; load latest bytecode
      large-file-warning-threshold        100000000                 ;; warn if file exceeds 100MB
      tab-always-indent                   'complete                 ;; tab completes
      indent-tabs-mode                    nil                       ;; don't use tabs
      require-final-newline               t                         ;; file ends in newline
      default-input-method                "MacOSX"                  ;; macOS comfort
      confirm-nonexistent-file-or-buffer  nil                       ;; don't annoy me with questions
      save-interprogram-paste-before-kill t
      echo-keystrokes                     0.001                     ;; hints show up in echo area faster
      make-backup-files                   nil                       ;; don't pollute the filesystem
      next-error-highlight                t                         ;; highlight until next command/location
      next-error-highlight-no-select      t                         ;; highlight indefinitely until replaced
      query-replace-highlight             t                         ;; highlight matches during query replace
      sentence-end-double-space           nil                       ;; this was always stupid
      shift-select-mode                   nil                       ;; don't mess with the mark
      transient-mark-mode                 t                         ;; regions are temporary like most apps
      truncate-partial-width-windows      nil                       ;; respect value of 'truncate-lines' variable
      vc-follow-symlinks                  t                         ;; symlinks aren't second-class citizens
      max-specpdl-size                    2400                      ;; limit on number of variable bindings
      ring-bell-function                  'ignore
      sh-learn-basic-offset               t                         ;; try to figure out offset for shell mode      
      locale-coding-system                'utf-8                    ;; utf-8 character encoding

      )

(setq-default initial-major-mode (quote emacs-lisp-mode))
(setq-default initial-scratch-message nil)

(fset 'yes-or-no-p 'y-or-n-p)

;;; --- character encoding ---

(prefer-coding-system        'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

;; --- general settings ---

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill)

  ;; prevent emacs error due to macos' ls not supporting long args...
  ;; ls does not support --dired; see ‘dired-use-ls-dired’
  ;; requires "brew install coreutils"
  (let ((ls-program "/usr/local/bin/gls"))
    (when (file-exists-p ls-program)
      (setq insert-directory-program ls-program))))

(delete-selection-mode) ;; delete selections with a keypress

(provide 'romney-settings)
;;; romney-settings.el ends here
