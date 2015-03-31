;;; personal/prelude-erc --- Summary
;;;
;;; Commentary:
;;;
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
(use-package erc-services
  :config
  (erc-services-mode 1))

;; Set erc-prompt-for-password to nil to use
;; $HOME/.authinfo.gpg encrypted credentials
(use-package erc-mode
  :init
  (setq whitespace-global-modes '(not erc-mode))
  (setq erc-prompt-for-password nil) 
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#clojure")
          ("freenode.net" "#racket")
          ("freenode.net" "#minikanren")
          ("freenode.net" "#hoplon")))
  (defun personal/connect-to-freenode ()
    (interactive)
    (erc :server "irc.freenode.net"
         :nick "cromney"
         :full-name "Christian Romney"))
  :bind ("<f9>" . personal/connect-to-freenode))

(use-package erc-match
  :init
  (setq erc-keywords '("cromney" "pointslope" "romney")
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-interpret-mirc-color t)
  :config
  (erc-match-mode 1))

(use-package erc-tweet
  :init
  (add-to-list 'erc-modules 'tweet)
  :config
  (erc-update-modules))

(provide 'personal/prelude-erc)
;;; prelude-erc.el ends here
