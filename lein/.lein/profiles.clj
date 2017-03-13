;; ~/.lein/profiles.clj
{:user {:plugins [[venantius/ultra "0.5.1"]
                  [com.jakemccrary/lein-test-refresh "0.16.0"]
                  [lein-pprint "1.1.2"]
                  [lein-try "0.4.3"]
                  [lein-ancient "0.6.8" :exclusions [org.clojure/clojure]]
                  [lein-hiera "0.9.5"]
                  [lein-vanity "0.2.0"]
                  [lein-kibit "0.1.3"]
                  [lein-annotations "0.1.0"]]}
 :repl {:plugins [[cider/cider-nrepl "0.14.0"]
                  [com.billpiel/sayid "0.0.11"]
                  [refactor-nrepl "2.2.0"]]}}
