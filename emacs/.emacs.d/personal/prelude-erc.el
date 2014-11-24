;;; prelude-erc --- Summary

;;; Commentary:

;; For this to work, you must have GPG Suite for Mac installed
;; (or be running Linux where gpg-agent just works)
;; then, from Emacs, create a $HOME/.authinfo.gpg file
;; with your info e.g.
;; machine irc.freenode.net login cromney password <pick-a-good-pass> port 6667
;; when you write the file, Emacs should prompt you for a public key to
;; encrypt the file for (choose *your* GPG key). Your $HOME/.authinfo.gpg should
;; now be encrypted (check using 'cat'). Kill the buffer then re-open.
;; It should prompt you for your GPG private key's passphrase. When you
;; enter it correctly, it will decrypt the file and show it to you.

;;; Code:
(require 'erc-services nil t)
(erc-services-mode 1)

;; Set to nil to use $HOME/.authinfo.gpg encrypted credentials
(setq erc-prompt-for-password nil)
(setq whitespace-global-modes '(not erc-mode))
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure")
        ("freenode.net" "#racket")
        ("localhost" "#clojure")))

(require 'erc-match)
(setq erc-keywords '("cromney" "pointslope"))
(erc-match-mode)

(require 'erc-tweet)
(add-to-list 'erc-modules 'tweet)
(erc-update-modules)

;; F9 - Connect to freenode irc directly
(global-set-key (kbd "<f9>")
  (lambda ()
    (interactive)
    (erc :server "irc.freenode.net"
         :nick "cromney"
         :full-name "Christian Romney")))

(provide 'personal-prelude-erc)
;;; prelude-erc.el ends here
