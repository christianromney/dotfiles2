;; ~/.lein/profiles.clj
{:user {:plugins [;; basic necessities
                  [cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  ;; refactoring support
                  [refactor-nrepl "0.2.2"]
                  ;; pretty print output
                  [lein-pprint "1.1.2"]
                  ;; shows nicer test output (diff between actual/expected)
                  [lein-difftest "2.0.0"]
                  ;; try libs before including them for realz
                  [lein-try "0.4.3"]
                  ;; check for outdated dependencies
                  [lein-ancient "0.6.1" :exclusions [org.clojure/clojure]]
                  ;; working with core.typed
                  [lein-typed "0.3.5"]
                  ;; linter
                  [jonase/eastwood "0.1.5" :exclusions [org.clojure/clojure]]
                  ;; autotest runner
                  [com.jakemccrary/lein-test-refresh "0.5.2" :exclusions [org.clojure/clojure]]
                  ;; uses graphviz to make a namespace hierarchy graphA
                  [lein-hiera "0.9.0"]
                  ;; lines of code, test code stats
                  [lein-vanity "0.2.0"]
                  ;; parses, prints TODOs
                  [lein-annotations "0.1.0"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
