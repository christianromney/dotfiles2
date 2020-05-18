{;; -- omnipresent --
 :user {:plugins [[lein-ancient "0.6.15"]
                  [lein-pprint "1.2.0"]]}

 ;; -- tooling --
 :depviz {:plugins [[lein-ns-dep-graph "0.2.0-SNAPSHOT"]]}

 :scope {:dependencies
         [[vvvvalvalval/scope-capture-nrepl "0.3.1"]]
         :repl-options
         {:nrepl-middleware
          [sc.nrepl.middleware/wrap-letsc]}}

 :cider {:plugins [[org.clojure/tools.nrepl "0.2.13"]
                   [cider/cider-nrepl "0.24.0" :exclusions [org.clojure/tools.nrepl]]
                   [refactor-nrepl "2.5.0" :exclusions [org.clojure/tools.logging]]
                   ]}

 :rebl {:resource-paths ["/Users/christian/.lein/REBL-0.9.220/REBL-0.9.220.jar"]
        :dependencies [[cljfmt "0.6.4"]
                       [org.openjfx/javafx-fxml     "11.0.1"]
                       [org.openjfx/javafx-controls "11.0.1"]
                       [org.openjfx/javafx-swing    "11.0.1"]
                       [org.openjfx/javafx-base     "11.0.1"]
                       [org.openjfx/javafx-web      "11.0.1"]]}

 :doc {:plugins [[lein-codox "0.10.7"]]}

 ;; -- project-specific profiles --
 :salk {:dependencies [[figwheel-sidecar "0.5.18" :exclusions [commons-codec ring/ring-core]]
                       [cider/piggieback "0.4.0" :exclusions [org.clojure/tools.logging]]
                       ]
        :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

 }
