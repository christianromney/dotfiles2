;;; .emacs --- My emacs configuration
;;; Commentary:
;;
;;; Code:
;;; performance settings

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6)

(add-to-list 'load-path "~/.emacs.d/config")

(require 'romney-directories)
(require 'romney-settings)
(require 'romney-built-ins)
(require 'romney-packages)
(require 'romney-general)
(require 'romney-theme)
(require 'romney-coding)
(require 'romney-clojure)
(require 'romney-web)
(require 'romney-org)
(require 'romney-keybindings)
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;;; .emacs ends here
