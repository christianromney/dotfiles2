;;; ../src/open/dotfiles/doom/dot-doom.d/+clojure.el -*- lexical-binding: t; -*-
;; ===============================================================================
;;                              INF-CLOJURE
;; ===============================================================================
(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot" "deps.edn"))

;; Large clojure buffers tend to be slower than large buffers of other modes, so
;; it should have a lower threshold too.
(add-to-list 'doom-large-file-size-alist '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

(defun +my/read-file-as-string (path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))

(defun +my/port-open-p (port)
  "Returns t if the given port is in use, nil otherwise."
  (= 0 (call-process "lsof" nil nil nil "-P" "-i" (concat "TCP:" (number-to-string port)))))

(defun +my/pretty-print ()
  "Pretty print the last repl output"
  (interactive)
  (comint-proc-query (inf-clojure-proc)
                     "(clojure.pprint/pprint *1)\n"))

(use-package! clojure-mode
  :hook (clojure-mode . rainbow-delimiters-mode)
  :config
  (when (featurep! :tools lsp)
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
        (push dir lsp-file-watch-ignored)))

    (setq lsp-diagnostic-package :none
          lsp-enable-snippet nil
          lsp-file-watch-threshold 2000))

  (defun +clojure-socket-repl-connect ()
    (interactive)
    (let ((default-socket-repl-port 5555)
          (path
           (expand-file-name (concat (projectile-project-root) ".shadow-cljs/socket-repl.port"))))
      (message "Attempting to connect to socket repl")
      (cond
       ;; option 1: check for shadow-cljs ephemeral port file
       ((file-exists-p path)
        (progn
          (message "Detected shadow port file: %s" path)
          (let ((port (+my/read-file-as-string path)))
            (message "Connecting to shadow socket repl on port %d" port)
            (inf-clojure-connect "localhost" port))))

       ;; option 2: check default port
       ((+my/port-open-p default-socket-repl-port)
        (progn
          (message "Connecting to default socket repl port %d" default-socket-repl-port)
          (inf-clojure-connect "localhost" default-socket-repl-port)))

       ;; option 3: ask me
       t
       (progn
         (message "Connecting to REPL interactively")
         (inf-clojure-connect)))))

  (map! :map clojure-mode-map
        "C-c r c"    #'+clojure-socket-repl-connect

        ;; connections
        "C-c j c"    #'inf-clojure
        "C-c j C"    #'inf-clojure-connect
        ;; docs
        "C-c j d"    #'lsp-ui-doc-glance
        "C-c j D"    #'inf-clojure-show-var-documentation

        ;; imenu
        "C-c j i"    #'lsp-ui-imenu

        ;; eval
        "C-c j e b"  #'inf-clojure-eval-buffer
        "C-c j e d"  #'inf-clojure-eval-defun
        "C-c j e D"  #'inf-clojure-eval-defun-and-go
        "C-c j e f"  #'inf-clojure-eval-last-sexp
        "C-c j e F"  #'inf-clojure-eval-form-and-next
        "C-c j e r"  #'inf-clojure-eval-region
        "C-c j e R"  #'inf-clojure-eval-region-and-go

        ;; misc
        "C-c j a"    #'inf-clojure-apropos
        "C-c j l"    #'inf-clojure-arglists
        "C-c j m"    #'inf-clojure-macroexpand
        "C-c j v"    #'inf-clojure-show-ns-vars

        ;; CIDER-like mappings
        "C-c M-n"    #'inf-clojure-set-ns
        "C-c M-p"    #'+my/pretty-print
        "C-c C-e"    #'inf-clojure-eval-last-sexp
        "C-x C-e"    #'inf-clojure-eval-last-sexp
        "C-c C-z"    #'inf-clojure-switch-to-repl
        "C-c C-k"    #'inf-clojure-load-file

        :map inf-clojure-mode-map
        "C-c C-,"    #'inf-clojure-clear-repl-buffer))

(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojure-mode-hook #'subword-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurec-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurex-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'inf-clojure-mode-hook #'turn-on-smartparens-strict-mode)

(use-package! flycheck-clj-kondo
  :when (featurep! :checkers syntax)
  :after flycheck)

;; ================================================================================
;;                                   REBL Support
;; ================================================================================
;; ;; Similar to C-x C-e, but sends to REBL
;; (defun rebl-eval-last-sexp ()
;;   (interactive)
;;   (let* ((bounds (cider-last-sexp 'bounds))
;;          (s (cider-last-sexp))
;;          (reblized (concat "(cognitect.rebl/inspect " s ")")))
;;     (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; ;; Similar to C-M-x, but sends to REBL
;; (defun rebl-eval-defun-at-point ()
;;   (interactive)
;;   (let* ((bounds (cider-defun-at-point 'bounds))
;;          (s (cider-defun-at-point))
;;          (reblized (concat "(cognitect.rebl/inspect " s ")")))
;;     (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; (map! :map clojure-mode-map
;;       "<f5>"    #'cider-jack-in
;;       "M-<f5>"  #'cider-jack-in-clj&cljs
;;       :map cider-mode-map
;;       "C-s-x"   #'rebl-eval-defun-at-point
;;       "C-x C-r" #'rebl-eval-last-sexp)

(message "Loaded +clojure configuration")
