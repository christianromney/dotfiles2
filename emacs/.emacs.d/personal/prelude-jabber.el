(use-package seq
  :ensure t)

(use-package request
  :ensure t)


(use-package jabber
  :ensure t
  :config
  (require 's)
  (require 'request)
  (require 'seq)
  (require 'jabber))

(defvar hipchat--user-info-for-completion nil)

(defcustom hipchat-nickname "Full name" "Hipchat real name (see see https://hipchat.com/account/xmpp)."
  :type '(string)
  :group 'hipchat)

(defcustom hipchat-autojoin nil "List of rooms to autojoin."
  :type '(repeat string)
  :group 'hipchat)

(defcustom hipchat-api-key nil "API key for hipchat.

Used for @ mention autocompletion.  Get via hipchat.com / Account
settings / API access.  Must have view_group scope."
  :type '(string)
  :group 'hipchat)

(defun hipchat-get-jid ()
  "Return the JID of the users hipchat account."
  (car (seq-elt (seq-filter (lambda (acct)
                              (s-match "chat.hipchat.com" (car acct)))
                            jabber-account-list) 0)))

(defun hipchat-get-connection ()
  "Return the hipchat jabber connetion."
  (jabber-find-connection (hipchat-get-jid)))

(defun hipchat-list ()
  "Open a list of all hipchat rooms."
  (interactive)
  (jabber-get-disco-items (hipchat-get-connection) "conf.hipchat.com" nil))

(defun hipchat-join-raw (groupid)
  "Join the hipchat group GROUPID."
   (jabber-muc-join (hipchat-get-connection) groupid hipchat-nickname t))

(defun hipchat-join ()
  "Prompt for hipchat group and join it."
  (interactive)
  (jabber-disco-get-items
   (hipchat-get-connection) "conf.hipchat.com" nil
   (lambda (jc data items)
     (let* ((groups (mapcar (lambda (item)
                              (cons (elt item 0) (elt item 1))) items))
            (groupname (completing-read "Room name: " groups))
            (groupid (cdr (assoc groupname groups))))
       (if (y-or-n-p (format "Autojoin %s in the future? " groupname))
           (customize-push-and-save 'hipchat-autojoin (list groupid)))
       (hipchat-join-raw groupid)))
   nil))

(defun hipchat-load-user-info ()
  "Load user information from hipchat API for autocompletion."
  (request
   "https://api.hipchat.com/v2/user"
       :params `(("auth_token" . ,hipchat-api-key)
                 ("max-results" . "500"))
       :parser 'json-read
       :success (function*
                 (lambda (&key data &allow-other-keys)
                   (setq hipchat--user-info-for-completion nil)
                   (seq-map (lambda (user)
                              (push (cons (format "@%s" (cdr (assq 'mention_name user)))
                                          (cdr (assq 'name user)))
                                    hipchat--user-info-for-completion))
                            (cdr (assq 'items data)))
                   (setq hipchat--user-info-for-completion
                         (completion-table-case-fold hipchat--user-info-for-completion))))))

(defun hipchat-setup-hook (jc)
  "Hook to run to setup hipchat.  Add to `jabber-post-connect-hooks'."
  (when (eq jc (hipchat-get-connection))
    (hipchat-load-user-info)
    (mapcar (lambda (id)
              (hipchat-join-raw id))
            hipchat-autojoin)))

(defun hipchat-setup-completion ()
  "Setup hipchat @ mention completion.  Add to `jabber-chat-mode-hook'."
  (define-key jabber-chat-mode-map [?\t] 'completion-at-point)
  (add-to-list 'completion-at-point-functions #'hipchat-complete-mention-function))

(defun hipchat-complete-mention-function (&optional start)
  "Completion at point function for hipchat @ mention completion."
  (let* ((end (point))
         (start
          (or start
              (save-excursion
                (re-search-backward "@")
                (point)))))
    (list start end hipchat--user-info-for-completion)))

(add-hook 'jabber-post-connect-hooks #'hipchat-setup-hook)
(add-hook 'jabber-chat-mode-hook #'hipchat-setup-completion)

;; use this if you don't like all those notices about people joining/leaving
(defadvice jabber-muc-process-presence
    (after jabber-muc-process-presence-clear-notices)
  "Remove all muc notices."
  (let* ((from (jabber-xml-get-attribute presence 'from))
	 (group (jabber-jid-user from))
         (buffer (get-buffer (jabber-muc-get-buffer group))))
    (if buffer
        (with-current-buffer buffer
          (ewoc-filter jabber-chat-ewoc (lambda (elt) (not (eq (car elt) :muc-notice))))))))

(ad-activate #'jabber-muc-process-presence)

;; Render HTML. Especially useful for notifications.
(when (functionp #'shr-insert-document)
  (defun jabber-chat-html-body (xml-data who mode)
    "Render HTML content in jabber buffer."
    (if (string-equal "html" (nth 2 (assq 'message_format (cdr (assq 'x (cdr xml-data))))))
        (if (eq mode :printp)
            t
          (let ((parsed (with-temp-buffer
                          (insert (nth 1 (cdr (assq 'body (cdr xml-data)))))
                          (libxml-parse-html-region (point-min) (point-max)))))
            (shr-insert-document parsed)
            t))
      nil))

  (add-to-list 'jabber-body-printers #'jabber-chat-html-body))

(provide 'prelude-jabber)
