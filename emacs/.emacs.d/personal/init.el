(prelude-require-packages
 '(;; General
   git-gutter-fringe+
   smart-mode-line
   writegood-mode
   dired+
   ace-jump-buffer
   ace-window
   ctags
   ctags-update
   indicators
   company
   multi-term
   erc-tweet
   sunshine
   ack

   ;; Clojure
   javadoc-lookup
   4clojure
   clojure-cheatsheet
   clojure-snippets
   datomic-snippets
   cljsbuild-mode
   align-cljlet
   clj-refactor
   typed-clojure-mode
   rainbow-identifiers

   ;; Racket / Scheme
   geiser

   ;; Misc Modes
   arduino-mode
   swift-mode
   terraform-mode
   nginx-mode
   puppet-mode
   adoc-mode
   
   ;; Themes
   display-theme
   flatland-theme
   
   ;; Org
   deft
   org2blog
   
   ;; Web 
   company-restclient
   restclient
   less-css-mode
   emmet-mode
   web-mode
   htmlize

   ;; Ruby / Rails
   projectile-rails
   flymake-ruby
   rbenv
   rspec-mode
))

;; Who am I
(setq user-email-address "cromney@pointslope.com")
(setq user-full-name "Christian Romney")

;; Where am I
(setq calendar-latitude 26.017)
(setq calendar-longitude -80.34110799999999)
(setq calendar-location-name "Pembroke Pines, FL")
(setq sunshine-location calendar-location-name)
