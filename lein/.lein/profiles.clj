{;;:user {:dependencies [[refactor-nrepl "2.4.0-SNAPSHOT" :exclusions [org.clojure/tools.nrepl]]]}

 :salk {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                  [cider/cider-nrepl "0.18.0" :exclusions [org.clojure/tools.nrepl org.clojure/clojure]]
                  [refactor-nrepl "2.4.0" :exclusions [org.clojure/tools.nrepl org.clojure/clojure]]]
        :dependencies [[figwheel-sidecar "0.5.16" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.3.9"]]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.18.0" :exclusions [org.clojure/tools.nrepl org.clojure/clojure]]
                   [refactor-nrepl "2.4.0" :exclusions [org.clojure/tools.nrepl org.clojure/clojure]]]}

 :tools {:plugins [[lein-ancient "0.6.15"]]}}
