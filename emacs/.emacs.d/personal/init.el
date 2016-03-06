;; Personal information
(setq user-email-address "cromney@pointslope.com")
(setq user-full-name "Christian Romney")
(setq calendar-latitude 26.017)
(setq calendar-longitude -80.34110799999999)
(setq calendar-location-name "Pembroke Pines, FL")

;; All other packages will be installed via use-package
(prelude-require-packages
 '(use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
