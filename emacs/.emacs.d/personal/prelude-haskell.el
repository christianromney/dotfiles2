(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Settings
(setq haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

;; Options for haskell-process-type: 
;; see: https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup
;; ghci (default)
;; cabal-repl
;; cabal-dev
;; cabal-ghci
(setq haskell-process-type 'ghci)
