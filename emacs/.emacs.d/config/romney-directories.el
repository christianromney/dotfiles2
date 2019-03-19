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

(provide 'romney-directories)
;;; romney-directories.el ends here
