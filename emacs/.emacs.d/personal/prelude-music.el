(use-package volume
  :ensure t)

(use-package bongo
  :ensure t
  :config
  (require 'volume)
  :bind (("<f8>" . bongo)
         ("<f9>" . bongo-pause/resume)))

(setq bongo-default-directory "~/Music/mp3")
