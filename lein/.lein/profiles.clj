{;; -- omnipresent --
 :user {:plugins [[lein-ancient "0.6.15"]
                  [lein-pprint "1.2.0"]
                  ]}

 ;; -- tooling --
 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.22.4" :exclusions [org.clojure/tools.nrepl]]
                   ;;[refactor-nrepl "2.4.0" :exclusions [org.clojure/tools.logging]]
                   ]}

 :doc {:plugins [[lein-codox "0.10.7"]]}

 ;; -- project-specific profiles --
 :salk {:dependencies [[figwheel-sidecar "0.5.18" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.4.0" :exclusions [org.clojure/tools.logging]]
                       ]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 }
