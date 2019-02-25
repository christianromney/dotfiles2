{;; -- omnipresent --
 :user {:plugins [[lein-ancient "0.6.15"]]}

 ;; -- tooling --
 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.21.1" :exclusions [org.clojure/clojure
                                                            org.clojure/tools.nrepl]]
                   [refactor-nrepl "2.4.0" :exclusions [org.clojure/clojure
                                                        nrepl
                                                        org.clojure/tools.logging
                                                        org.clojure/tools.nrepl]]]}

 ;; -- project-specific profiles --
 :salk {:dependencies [[figwheel-sidecar "0.5.18" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.4.0" :exclusions [org.clojure/tools.logging]]]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 }
