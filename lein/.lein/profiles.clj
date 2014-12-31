;; ~/.lein/profiles.clj
{:user {:plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [refactor-nrepl "0.2.2"]
                  [lein-pprint "1.1.2"]
                  [lein-difftest "2.0.0"]
                  [lein-try "0.4.3"]
                  [lein-typed "0.3.5"]
                  [jonase/eastwood "0.1.5"]
                  [com.jakemccrary/lein-test-refresh "0.5.2"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
