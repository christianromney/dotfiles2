;;; package --- Summary
;;; Commentary:
;;;
;;; define directories that will be used by other configuration settings.
;;;
;;; Code:
(defun ensure-directory (dir)
  "Create a directory (given as DIR) if it doesn't already exist."
  (unless (file-exists-p dir)
  (make-directory dir)))

(defconst personal-savefile-dir
  (expand-file-name "savefile" user-emacs-directory))

(defconst personal-backup-dir
  (expand-file-name "backups" personal-savefile-dir))

(defconst personal-autosave-dir
  (expand-file-name "autosave" personal-savefile-dir))

(defconst personal-config-dir
  (expand-file-name ".emacs.d/config" (file-name-directory user-init-file)))

(ensure-directory personal-savefile-dir)
(ensure-directory personal-backup-dir)
(ensure-directory personal-autosave-dir)

(provide 'romney-directories)
;;; romney-directories.el ends here
