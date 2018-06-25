{:salk {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                  [cider/cider-nrepl "0.17.0" :exclusions [org.clojure/tools.nrepl]]
                  [refactor-nrepl "2.4.0-SNAPSHOT" :exclusions [org.clojure/tools.nrepl]]]
        :dependencies [[figwheel-sidecar "0.5.16" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.3.3"]]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 :bengal {:plugins [[lein-ancient "0.6.15"]
                    [cider/cider-nrepl "0.17.0" :exclusions [org.clojure/tools.nrepl]]
                    [refactor-nrepl "2.4.0-SNAPSHOT"]]}

 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.17.0" :exclusions [org.clojure/tools.nrepl]]
                   [refactor-nrepl "2.4.0-SNAPSHOT" :exclusions [org.clojure/tools.nrepl]]]}
 :tools {:plugins [[lein-ancient "0.6.15"]]}}
