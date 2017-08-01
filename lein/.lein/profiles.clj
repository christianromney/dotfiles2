;; ~/.lein/profiles.clj
{:user {:figwheel {:nrepl-middleware ["cider.nrepl/cider-middleware"
                                      "cemerick.piggieback/wrap-cljs-repl"]}
        :dependencies [[figwheel-sidecar "0.5.10"]
                       [org.clojure/tools.nrepl "0.2.13"]]
        :plugins [[com.jakemccrary/lein-test-refresh "0.20.0"]
                  [lein-pprint "1.1.2"]
                  [lein-ancient "0.6.10"]
                  [lein-try "0.4.3"]
                  [lein-kibit "0.1.6-beta1" :exclusions [org.clojure/clojure]]
                  [refactor-nrepl "2.3.1" :exclusions [org.clojure/tools.nrepl]]
                  [cider/cider-nrepl "0.15.0" :exclusions [org.clojure/tools.nrepl]]

                  ;;[com.billpiel/sayid "0.0.15" :exclusions [org.clojure/clojure]]
                  ]}}
