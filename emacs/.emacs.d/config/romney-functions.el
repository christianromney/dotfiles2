;;; romney-functions.el --- My function definitions
;;; Commentary:
;;; My custom EmacsLisp functions
;;; Code:
;; -- window management -
(defun romney/find-window-by-name (window-name)
  "Find a window with the given name.
WINDOW-NAME - string name of the window to return"
  (get-window-with-predicate
   (lambda (w)
     (string-equal window-name (buffer-name (window-buffer w))))))

(defun romney/kill-window-by-name (window-name)
  "Kill the window with the given name.
WINDOW-NAME - string name of the window to kill"
  (quit-window t (romney/find-window-by-name window-name)))

(defun romney/ensure-directory (dir &optional add-to-load-path-p)
  "Create a directory and its parents (given as DIR) if it doesn't already exist."
  (unless (file-exists-p dir)
    (make-directory dir t))

  (when add-to-load-path-p
    (add-to-list 'load-path dir))

  dir)

(provide 'romney-functions)
;;; romney-functions.el ends here
