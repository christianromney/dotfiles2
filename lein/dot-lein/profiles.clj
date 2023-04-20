{;; -- omnipresent --
 :user {:plugins [[lein-ancient "0.6.15"]
                  [lein-pprint "1.3.2"]]}


 :socket {:jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]
          :dependencies [[compliment/compliment "0.3.12"]]}

 ;; -- tooling --
 :depviz  {:plugins [[lein-ns-dep-graph "0.2.0-SNAPSHOT"]]}
 :datomic {:dependencies [[com.datomic/dev-local "0.9.195"]]}
 :scope {:dependencies [[vvvvalvalval/scope-capture-nrepl "0.3.1"]
                        [jsofra/data-scope "0.1.2"]]
         :injections [(require 'data-scope.charts)
                      (require 'data-scope.graphs)
                      (require 'data-scope.inspect)
                      (require 'data-scope.pprint)]
         :repl-options
         {:nrepl-middleware
          [sc.nrepl.middleware/wrap-letsc]}}

 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.25.3" :exclusions [org.clojure/tools.nrepl]]
                   [refactor-nrepl "2.5.0" :exclusions [org.clojure/tools.logging]]
                   ]}

 :rebl {:dependencies [[com.cognitect/rebl "0.9.241"]]}

 :doc {:plugins [[lein-codox "0.10.7"]]}

 ;; -- project-specific profiles --
 :salk {:dependencies [[figwheel-sidecar "0.5.20" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.5.1" :exclusions [org.clojure/tools.logging]]
                       ]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 }
