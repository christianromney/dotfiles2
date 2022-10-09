;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el

;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)

;; ------------------------------------------------------------------------------
;; Appearance
;; ------------------------------------------------------------------------------
(package! ef-themes :recipe (:host github :repo "protesilaos/ef-themes"))
(package! svg-tag-mode :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! pulsar :recipe (:host github :repo "protesilaos/pulsar"))
(package! xterm-color)

;; ------------------------------------------------------------------------------
;;  Git extras
;; ------------------------------------------------------------------------------
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))
(package! git-link)

;; ------------------------------------------------------------------------------
;;  Utilities
;; ------------------------------------------------------------------------------
(package! adoc-mode)
(package! consult-company)
(package! consult-yasnippet)
(package! dirvish)
(package! free-keys :recipe (:host github :repo "Fuco1/free-keys"))
(package! google-this)
(package! helpful)
(package! reveal-in-osx-finder)
;; ------------------------------------------------------------------------------
;;  Clojure support
;; ------------------------------------------------------------------------------
(package! clojure-snippets)
(package! inf-clojure :pin "c23c55e662c16ec94e684b2ec9611e78531921a8")
(when (modulep! :checkers syntax)
  (package! flycheck-clj-kondo :pin "a558bda44c4cb65b69fa53df233e8941ebd195c5"))

;; ------------------------------------------------------------------------------
;;  Org mode
;; ------------------------------------------------------------------------------
(package! brazilian-holidays)
(package! graphviz-dot-mode)
(package! org-modern)
(package! org-super-agenda)
(package! org-auto-tangle)
(package! org-glossary :recipe (:host github :repo "tecosaur/org-glossary"))
(package! zotxt) ;; zotero bibliography support C-c " "

;; -----------------------------------------------------------------------------
;; Prolog
;; ------------------------------------------------------------------------------
(package! ediprolog) ;; https://github.com/triska/ediprolog
(package! ob-prolog)
;; ------------------------------------------------------------------------------
;; C
;; ------------------------------------------------------------------------------
(when (modulep! :lang cc)
  (package! disaster :recipe (:host github :repo "jart/disaster")))
