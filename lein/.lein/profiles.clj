;; ~/.lein/profiles.clj
{:user {:plugins [[cider/cider-nrepl "0.8.0"]
                  [refactor-nrepl "0.1.0"]
                  [lein-pprint "1.1.2"]
                  [lein-difftest "2.0.0"]
                  [lein-try "0.4.3"]
                  [lein-typed "0.3.5"]
                  [jonase/eastwood "0.1.5"]
                  [com.aphyr/prism "0.1.1"]]
        :dependencies [[pjstadig/humane-test-output "0.6.0"]
                       [com.aphyr/prism "0.1.1"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}}
