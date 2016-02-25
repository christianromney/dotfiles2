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
  :bind
  ("<f5>" . pivotal))

(provide 'prelude-pivotal)


(setq pivotal-api-token
      (getenv "PIVOTAL_API_TOKEN"))
