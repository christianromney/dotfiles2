;; -*- lexical-binding: t; -*-

;; Geiser is a nightmare with mit-scheme
(add-hook! 'scheme-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook! 'scheme-mode-hook (lambda () (require 'xscheme)))
(map! :map scheme-mode-map
      "C-c C-b" #'xscheme-send-buffer
      "C-c C-e" #'xscheme-send-previous-expression
      "C-c C-r" #'xscheme-send-region
      "C-c C-z" #'xscheme-select-process-buffer
      "C-c C-c" #'xscheme-send-control-g-interrupt
      "C-c I x" #'xscheme-send-control-x-interrupt
      "C-c I u" #'xscheme-send-control-u-interrupt
      "C-c I b" #'xscheme-send-breakpoint-interrupt
      "C-c I p" #'xscheme-send-proceed)
