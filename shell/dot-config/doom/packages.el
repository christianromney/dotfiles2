;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;; -----------------------------------------------------------------------------
;; Appearance
;; -----------------------------------------------------------------------------
(package! pulsar :recipe (:host github :repo "protesilaos/pulsar"))

;; ------------------------------------------------------------------------------
;; Languages
;; ------------------------------------------------------------------------------
(package! adoc-mode)
(package! inf-clojure :pin "b153e5126419910c38691088aab569b7c281068c")
(when (modulep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "ff7bed2315755cfe02ef471edf522e27b78cd5ca"))
(package! clojure-snippets)

;; -----------------------------------------------------------------------------
;; Open AI
;; -----------------------------------------------------------------------------
(package! gptel)
(package! greader)
(package! openai  :recipe (:host github :repo "emacs-openai/openai"))  ;; core
(package! codegpt :recipe (:host github :repo "emacs-openai/codegpt")) ;; code
(package! dall-e  :recipe (:host github :repo "emacs-openai/dall-e"))  ;; draw
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
;; ------------------------------------------------------------------------------
;; Org
;; ------------------------------------------------------------------------------
(package! org-ai)
(package! org-modern)
(package! graphviz-dot-mode)
(package! brazilian-holidays)
(package! consult-org-roam :recipe(:host github :repo "jgru/consult-org-roam"))
(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary"))
(package! zotxt)

;; ------------------------------------------------------------------------------
;; Utilities
;; ------------------------------------------------------------------------------
(package! consult-company)
(package! consult-yasnippet)
(package! free-keys :recipe (:host github :repo "Fuco1/free-keys"))
(package! google-this)
