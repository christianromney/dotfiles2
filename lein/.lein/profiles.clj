;; ~/.lein/profiles.clj
{:user {:plugins [;; pretty print output
                  [lein-pprint "1.1.2"]
                  ;; Quick test runs
                  [quickie "0.4.1"]
                  ;; Midje tests
                  [lein-midje "3.1.3"]
                  ;; shows nicer test output (diff between actual/expected)
                  [lein-difftest "2.0.0"]
                  ;; try libs before including them for realz
                  [lein-try "0.4.3"]
                  ;; check for outdated dependencies
                  [lein-ancient "0.6.8" :exclusions [org.clojure/clojure]]
                  ;; working with core.typed
                  [lein-typed "0.3.5"]
                  ;; linter
                  [jonase/eastwood "0.2.2" :exclusions [org.clojure/clojure]]
                  ;; autotest runner                  
                  [com.jakemccrary/lein-test-refresh "0.12.0" :exclusions [org.clojure/clojure]]
                  ;; uses graphviz to make a namespace hierarchy graph
                  [lein-hiera "0.9.5"]
                  ;; lines of code, test code stats
                  [lein-vanity "0.2.0"]
                  ;; parses, prints TODOs
                  [lein-annotations "0.1.0"]]
        :dependencies [[pjstadig/humane-test-output "0.7.0"]
                       [flare "0.2.9"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)
                     (require 'flare.clojure-test)
                     (flare.clojure-test/install!)]
        :test-refresh {:quiet true}}
 :repl {:plugins [;; basic necessities
                  [cider/cider-nrepl "0.10.0"]
                  ;; refactoring support
                  [refactor-nrepl "2.0.0-SNAPSHOT"]]}}
