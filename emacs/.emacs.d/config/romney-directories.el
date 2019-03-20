;;; package --- Summary
;;; Commentary:
;;;
;;; define directories that will be used by other configuration settings.
;;;
;;; Code:
(defun romney/ensure-directory (dir)
  "Create a directory (given as DIR) if it doesn't already exist."
  (unless (file-exists-p dir)
  (make-directory dir)))

(defconst personal-config-dir
  (expand-file-name "config" user-emacs-directory))

(defconst personal-data-dir
  (expand-file-name "data" user-emacs-directory))

(defconst personal-bookmarks-dir
  (expand-file-name "bookmarks" personal-data-dir))

(defconst personal-org-dir
  (expand-file-name "org" personal-data-dir))

(defconst personal-org-file-agenda
  (expand-file-name "agenda.org" personal-org-dir))

(defconst personal-org-file-todo
  (expand-file-name "todos.org" personal-org-dir))

(defconst personal-org-file-notes
  (expand-file-name "notes.org" personal-org-dir))

(defconst personal-org-file-journal
  (expand-file-name "journal.org" personal-org-dir))

(defconst personal-org-file-cookbook
  (expand-file-name "cookbook.org" personal-org-dir))

(defconst personal-org-file-snippets
  (expand-file-name "snippets.org" personal-org-dir))

(defconst personal-org-file-default
  personal-org-file-todo)

(defconst personal-org-template-dir
  (expand-file-name "templates" personal-org-dir))

(defconst personal-org-template-journal
  (expand-file-name "journal.template" personal-org-template-dir))

(defconst personal-org-template-note
  (expand-file-name "note.template" personal-org-template-dir))

(defconst personal-org-template-quote
  (expand-file-name "quote.template" personal-org-template-dir))

(provide 'romney-directories)
;;; romney-directories.el ends here
