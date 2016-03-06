;; find unbound key combinations
(use-package unbound
  :ensure t)

;; restclient + autocompletion of headers
(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

;; Integrate w/ Pivotal Tracker projects
;; https://github.com/jxa/pivotal-tracker
;;
;; Key Bindings
;; =========================================
;;
;; viewing
;; -----------------------------------------
;; n         next
;; p         previous
;; o         open
;; t         toggle expanded view for story
;; <enter>   toggle expanded view for story
;; N         next iteration
;; P         previous iteration
;;
;; editing
;; -----------------------------------------
;; E         enter Estimate
;; C         enter Comment
;; S         enter Status
;; O         enter Owner
;; T         enter Task
;; F         Finished!
;; +         new story
(use-package pivotal-tracker
  :ensure t
  :defer t
  :config
  (setq pivotal-api-token
        (getenv "PIVOTAL_API_TOKEN"))
  :bind
  ("<f5>" . pivotal))

;; music
(use-package volume
  :ensure t)

(use-package bongo
  :ensure t
  :defer t
  :config
  (require 'volume)
  (define-bongo-backend afplay
    :pretty-name "afplay"
    :matcher '(local-file "mp3" "m4a"))
  (setq bongo-default-directory "~/Music/mp3")
  :bind (("<f8>" . bongo)
         ("<f9>" . bongo-pause/resume)))

(use-package hackernews
  :ensure t
  :defer t
  :bind (("<f6>" . hackernews)))
