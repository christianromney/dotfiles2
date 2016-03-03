(setq warning-minimum-level :error)

;; pin things right away
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(setq package-pinned-packages
      '((clojure-mode        . "melpa-stable")
        (cider               . "melpa-stable")
        (company             . "melpa-stable")
        (helm                . "melpa-stable")
        (helm-ag             . "melpa-stable")
        (helm-core           . "melpa-stable")
        (helm-descbinds      . "melpa-stable")
        (projectile          . "melpa-stable")
        (rainbow-delimiters  . "melpa-stable"))) 
