(prelude-require-packages
 '(solarized-theme
   git-gutter-fringe+
   smart-mode-line
   puppet-mode
   dired+
   javadoc-lookup
   ace-jump-buffer
   geiser
   ctags
   ctags-update
   csharp-mode
   less-css-mode
   erc-tweet
   4clojure
   clojure-cheatsheet
   clojure-snippets
   datomic-snippets
   arduino-mode
   align-cljlet
   emmet-mode
   cljsbuild-mode
   swift-mode
   indicators
   company
   terraform-mode
))

;; Make geiser work with org-babel scheme from http://samrat.me/blog/2013/06/using-geiser-with-org-babel/
;;(live-add-pack-lib "ob-scheme.el")

;; Whoami
(setq user-email-address "cromney@pointslope.com")
(setq user-full-name "Christian Romney")
;; Whereami
(setq calendar-latitude 26.017)
(setq calendar-longitude -80.34110799999999)
(setq calendar-location-name "Pembroke Pines, FL")
