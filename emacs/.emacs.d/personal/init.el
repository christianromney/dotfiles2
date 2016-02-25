(prelude-require-packages
 '(use-package
    dired+
    ack
    git-gutter-fringe+
    eww-lnum
    unbound
    javadoc-lookup
    datomic-snippets
    cljsbuild-mode
    align-cljlet
    rainbow-identifiers
    terraform-mode
    restclient
    less-css-mode
    htmlize
    company-restclient
    company-web
    company-quickhelp
    pivotal-tracker
    display-theme
    flatland-theme
    material-theme
    ample-theme))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq user-email-address "cromney@pointslope.com")
(setq user-full-name "Christian Romney")
(setq calendar-latitude 26.017)
(setq calendar-longitude -80.34110799999999)
(setq calendar-location-name "Pembroke Pines, FL")
(setq sunshine-location calendar-location-name)

