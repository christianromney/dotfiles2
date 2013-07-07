; From http://paradigmx.net/blog/2013/04/01/clojure-toolchain-reloaded/
; Use live-use-packs to control which built-in packs should be loaded
; The built-in live/colour-pack MUST be disabled if you want to use 3rd party color theme pack
(live-use-packs '(live/foundation-pack
                  live/git-pack
                  live/clojure-pack
                  live/lang-pack
                  live/power-pack
                  live/bindings-pack))

(live-add-packs '(~/.live-packs/install-packages-pack
                  ~/.live-packs/romney-pack
                  ))
