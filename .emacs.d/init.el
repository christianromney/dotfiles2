;; Personal Info
(setq user-email-address "cromney@pointslope.com")
(setq user-full-name "Christian Romney")

;; Package Management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-js
                      yasnippet
                      clojure-mode
                      align-cljlet
                      expand-region
                      rainbow-delimiters
                      fuzzy
                      popup
                      mic-paren
                      auto-complete
                      org
                      rinari
                      nrepl
                      nrepl-ritz)
  "This is the list of packages I like to have installed")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; For editing lisp code
(global-set-key (kbd "C-c e") 'eval-and-replace)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Access snippets from anywhere
(require 'yasnippet)
(yas-global-mode 1)

(require 'mic-paren)
(paren-activate)

;; Better indentation for Compojure routes
(require 'clojure-mode)
(require 'align-cljlet)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))


(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
  (require 'nrepl-ritz))

;(setenv "PATH" (concat "/usr/local/smlnj-110.75/bin:" (getenv "PATH")))
;(setq exec-path (cons "/usr/local/smlnj-110.75/bin" exec-path))

;; Fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Appearance Settings (theme, font, line numbers)
(color-theme-sanityinc-solarized-dark)

(set-face-attribute 'default nil :font "Menlo-18")
(global-linum-mode 1)
(setq linum-format " %d ")

;; Like Vim's Easy Motion
(define-key global-map (kbd "C-0") 'ace-jump-mode)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Tab/indentation settings
(setq c-basic-offset 2)
(setq tab-width 2)
(setq indent-tabs-mode nil)

(require 'rinari)

;;auto-complete mode
(require 'fuzzy)
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-disable-inline t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode scheme-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))


;;;;Key triggers
(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "M-RET") 'ac-help)
(define-key ac-completing-map "\r" 'nil)

;; Org mode
(require 'ob-clojure)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-agenda-files (list "~/org/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/Todo/cbe-master-todo.org" "/Users/christian/org/todo/todo.org")))
 '(send-mail-function nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Yegge's advice
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

