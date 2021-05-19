;;; ../src/open/dotfiles/doom/dot-doom.d/+clojure.el -*- lexical-binding: t; -*-
;; ===============================================================================
;;                              INF-CLOJURE
;; ===============================================================================
(after! projectile
  (pushnew! projectile-project-root-files "project.clj" "build.boot" "deps.edn"))

;; Large clojure buffers tend to be slower than large buffers of other modes, so
;; it should have a lower threshold too.
(add-to-list 'doom-large-file-size-alist '("\\.\\(?:clj[sc]?\\|dtm\\|edn\\)\\'" . 0.5))

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
        (add-to-list 'lsp-file-watch-ignored dir)))

    (setq lsp-lens-enable          t       ;; enable LSP code lens for inline reference counts
          lsp-file-watch-threshold 2000
          lsp-enable-snippet       t)

    (map! :map clojure-mode-map
          ;; docs
          "C-c j d"    #'lsp-ui-doc-glance

          ;; imenu
          "C-c j i"    #'lsp-ui-imenu)))

(use-package! inf-clojure
  :config
  (defun +inf-clojure-run-tests ()
    "Run clojure.test suite for the current namespace."
    (interactive)
    (comint-proc-query (inf-clojure-proc)
                        "(clojure.test/run-tests)\n"))


  (defun +inf-clojure-pretty-print ()
    "Pretty print the last repl output"
    (interactive)
    (comint-proc-query (inf-clojure-proc)
                       "(do \n(newline)\n(clojure.pprint/pprint *1))\n"))

  (defun +inf-clojure-load-file ()
    "Send a load-file instruction to Clojure to load the current file.
Uses comint-proc-query instead of comint-send-string like
inf-clojure does by default, as that method breaks REPLs for me
with large files for some reason."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (comint-proc-query
       (inf-clojure-proc)
       (format "(do (load-file \"%s\") :loaded)\n" file-name))
      (message "inf-clojure :: Loaded file: %s" file-name)))

  (defun +possible-project-file (relative-path)
    (if (not (string-blank-p (projectile-project-root)))
        (let ((path (expand-file-name (concat (projectile-project-root) relative-path))))
          (if (file-exists-p path) path nil))
      nil))

  (defun +inf-clojure-socket-repl-connect ()
    (interactive)
    (message "inf-clojure-socket-repl-connect in project %s" (projectile-project-root))
    (let ((default-socket-repl-port 5555)
          (found-port-file (+possible-project-file ".shadow-cljs/socket-repl.port")))
      (cond
       ;; option 1: check for shadow-cljs ephemeral port file
       (found-port-file
        (let ((port (custom/read-file-as-string found-port-file)))
          (message "Connecting clojure socket REPL on ephemeral shadow port %s" port)
          (inf-clojure (cons "localhost" port))))

       ;; option 2: check default port
       ((custom/port-open-p default-socket-repl-port)
        (progn
          (message "Connecting clojure socket REPL on detected open port %d" default-socket-repl-port)
          (inf-clojure (cons "localhost" default-socket-repl-port))))

       ;; option 3: ask me
       (t
        (progn
          (message "Connecting clojure socket REPL interactively")
          (inf-clojure-connect))))))

  (map! :map clojure-mode-map
        "C-c r c"    #'+inf-clojure-socket-repl-connect

        ;; connections
        "C-c j c"    #'inf-clojure
        "C-c j C"    #'inf-clojure-connect
        ;; docs
        "C-c j D"    #'inf-clojure-show-var-documentation

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
        "C-c j r"    #'inf-clojure-reload
        "C-c j R"    #'inf-clojure-restart
        "C-c j v"    #'inf-clojure-show-ns-vars
        "C-c j t"    #'+inf-clojure-run-tests

        ;; CIDER-like mappings
        "C-c M-j"    #'+inf-clojure-socket-repl-connect
        "C-c C-q"    #'inf-clojure-quit
        "C-c M-n"    #'inf-clojure-set-ns
        "C-c M-p"    #'+inf-clojure-pretty-print
        "C-c C-e"    #'inf-clojure-eval-last-sexp
        "C-x C-e"    #'inf-clojure-eval-last-sexp
        "C-c C-z"    #'inf-clojure-switch-to-repl
        "C-c C-k"    #'+inf-clojure-load-file
        "C-c ,"      #'inf-clojure-clear-repl-buffer

        :map inf-clojure-mode-map
        "C-c ,"      #'inf-clojure-clear-repl-buffer
        "C-c j R"    #'inf-clojure-restart))

(defun +inf-clojure-reconfigure ()
  (progn
    (message "Setting clojure completion mode to compliment")
    (inf-clojure-update-feature
     'clojure 'completion
     "(compliment.core/completions \"%s\")")))

(add-hook! 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojure-mode-hook #'subword-mode)
(add-hook! 'clojurescript-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurec-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'clojurex-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'inf-clojure-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'inf-clojure-mode-hook #'+inf-clojure-reconfigure)

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
