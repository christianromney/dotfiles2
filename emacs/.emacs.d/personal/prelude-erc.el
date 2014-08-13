(defun cr-read-file-contents (path)
  "Read a file and return its contents as a string"
  (with-temp-buffer
    (insert-file-contents path (current-buffer) nil nil nil)
    (buffer-string)))

(defun cr-credentials-for (authtype)
  "Get a particular type of credentials from .authinfo as a pair"
  (let* ((parts (first
                 (cl-remove-if-not
                  (apply-partially 'string-match-p authtype)
                  (split-string (cr-read-file-contents "~/.authinfo") "\n" t))))
         (token (split-string parts))
         (login (cadr (member "login" token)))
         (passw (cadr (member "password" token))))
    (cons login passw)))

(defun cr-credential-username (creds)
  "Return the username part of credentials"
  (car creds))

(defun cr-credential-password (creds)
  "Return the password part of credentials"
  (cdr creds))

(defun cr-connect-to-irc-server (server)
  "Connect to the specified IRC server using authinfo credentials"
  (let ((creds (cr-credentials-for "erc")))
    (erc :server server
         :port 6667
         :nick (cr-credential-username creds)
         :password (cr-credential-password creds)
         :full-name "Christian Romney")))

(setq whitespace-global-modes '(not erc-mode))

(require 'erc-services nil t)
(erc-services-mode 1)
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
    (cr-connect-to-irc-server "irc.freenode.net")))
