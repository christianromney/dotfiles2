;; ~/.lein/profiles.clj
{:user {:figwheel
        {:nrepl-middleware ["cider.nrepl/cider-middleware"
                            "cemerick.piggieback/wrap-cljs-repl"]}
        :dependencies [#_[figwheel-sidecar "0.5.14"]
                       [org.clojure/tools.nrepl "0.2.13"]
                       [vvvvalvalval/scope-capture "0.1.3"]
                       [pjstadig/humane-test-output "0.8.3"]
                       [datawalk "0.1.11"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :plugins [[com.jakemccrary/lein-test-refresh "0.21.1"]
                  [lein-pprint "1.1.2"]
                  [lein-ancient "0.6.14"]
                  [refactor-nrepl "2.3.1" :exclusions [org.clojure/tools.nrepl]]
                  [cider/cider-nrepl "0.15.1" :exclusions [org.clojure/tools.nrepl]]
                  [com.billpiel/sayid "0.0.15" :exclusions [org.clojure/clojure org.clojure/tools.reader]]]}}
