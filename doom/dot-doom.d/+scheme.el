;;; ../src/open/dotfiles/doom/dot-doom.d/+scheme.el -*- lexical-binding: t; -*-

(use-package! geiser
  :defer t
  :config
  (setq geiser-active-implementations '(mit chez racket)))
