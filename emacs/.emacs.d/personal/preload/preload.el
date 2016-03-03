(setq warning-minimum-level :error)

;; pin things right away
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(setq package-pinned-packages
      '((clojure-mode        . "melpa-stable")
	(cider               . "melpa-stable")
	(rainbow-delimiters  . "melpa-stable")
	(helm                . "melpa-stable")
	(company             . "melpa-stable")
	(projectile          . "melpa-stable"))) 
