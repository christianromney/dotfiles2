;;; ~/.doom.d/smartparens.el -*- lexical-binding: t; -*-

;; strict mode is the only way to fly
(after! smartparens (smartparens-global-strict-mode +1))
(after! smartparens
  (progn
    (defun personal-just-one-space ()
      "Command to delete all but one whitespace character."
      (interactive)
      (just-one-space -1))

    (defun personal-delete-horizontal-space ()
      "Command to delete all whitespace."
      (interactive)
      (just-one-space -1)
      (sp-backward-delete-char))

    (map! :map smartparens-mode-map
      "M-\\"    #'personal-delete-horizontal-space
      "M-SPC"   #'personal-just-one-space)))
