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

(provide 'romney-functions)
;;; romney-functions.el ends here
