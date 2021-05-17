;;; ../src/open/dotfiles/doom/dot-doom.d/+cc.el -*- lexical-binding: t; -*-

;; ===============================================================================
;;                                 C Programming
;; ===============================================================================
(when (featurep! :lang cc)
  (map! :map c-mode-base-map
        ;; disassembler (objdump)
        "C-c o a"    #'disaster))

;; disassembler
(use-package! disaster
  :config
  ;; the default -M att argument doesn't work for me using
  ;; Apple clang version 12.0.5 (clang-1205.0.22.9)
  ;; Target: x86_64-apple-darwin20.4.0
  (setq disaster-objdump "objdump -d -Sl --no-show-raw-insn"))
