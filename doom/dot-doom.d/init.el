;;; init.el -*- lexical-binding: t; -*-
;; ============================================================================
;;                                HELP
;; ============================================================================
;; This file controls what Doom module installation and load order.
;; NOTE Remember to run 'doom sync' after modifying this file!
;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).
;; ============================================================================
(doom! :completion
       company             ; the ultimate code completion backend
       (helm +fuzzy)       ; the *other* search engine for love and life

       :ui
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       minimap             ; show a map of the code on the side
       modeline            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink cursor line after big motions
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (pretty-code
        +iosevka)          ; uglify everything
       treemacs            ; a project drawer, like neotree but cooler
       unicode             ; extended unicode support for various languages
       vc-gutter           ; vcs diff in the fringe
       window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces

       :editor
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       ;lispy               ; vim for lisp, for people who don't like vim
       parinfer            ; turn lisp into python, sort of
       rotate-text         ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)      ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       (ibuffer +icons)    ; interactive buffer management
       undo                ; persistent, smarter undo for your inevitable mistakes
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm               ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       spell               ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       gist                ; interacting with github gists
       lookup              ; navigate your code and its documentation
       lsp
       macos               ; MacOS-specific commands
       (magit +forge)      ; a git porcelain for Emacs
       make                ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf                 ; pdf enhancements
       rgb                 ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       terraform           ; infrastructure as code

       :lang
       ;;cc                ; C/C++/Obj-C madness
       (clojure +lsp)      ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       data                ; config/data formats
       emacs-lisp          ; drown in parentheses
       ess                 ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;(go +lsp)         ; the hipster dialect
       ;;hy                ; readability of scheme w/ speed of python
       (json +lsp)         ; At least it ain't XML
       (java +meghanada)   ; the poster child for carpal tunnel syndrome
       (javascript +lsp)   ; all(hope(abandon(ye(who(enter(here))))))
       (julia +lsp)        ; a better, faster MATLAB
       (latex +lsp)        ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       markdown            ; writing docs for people to ignore
       (org                ; organize your plain life in plain text
        +dragndrop
        +gnuplot
        +hugo
        +journal
        +noter             ; sync notes with PDFs
        +pomodoro          ; jedi mind trick yourself into focusing
        +present)          ; includes org-re-reveal
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       plantuml            ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp)       ; beautiful is better than ugly
       (racket +xp)        ; a DSL for DSLs
       rest                ; Emacs as a REST client
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scheme              ; a fully conniving family of lisps
       (sh +fish +lsp)     ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)          ; the tubes
       (yaml +lsp)         ; JSON, but readable

       :app
       calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings))
