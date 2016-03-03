(use-package volume
  :ensure t)

(use-package bongo
  :ensure t
  :config
  (require 'volume)
  (define-bongo-backend afplay
    :pretty-name "afplay"
    :matcher '(local-file "mp3" "m4a"))
  :bind (("<f8>" . bongo)
         ("<f9>" . bongo-pause/resume)))

(setq bongo-default-directory "~/Music/mp3")
