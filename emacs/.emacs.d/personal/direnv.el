(require 's)
(require 'dash)
(require 'projectile)

;; Motivation:
;;
;; I want to launch Emacs as a GUI and have it automatically
;; set the same environment variables that direnv sets for me
;; at the shell when I am in a Projectile project.
;;
;; EXPERIMENTAL - WIP!!!
;; Known Issues:
;; - does not unload environment variables
;; - does not prompt for loading of variables

(defun direnv-parse-export (line)
  "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
  (let* ((parts (s-split "=" line))
         (varname (car (last (s-split " " (first parts)))))
         (varval (car (last parts))))
    (cons varname varval)))

(defun direnv-read-file-as-string (filename)
  "Returns a the file's contents as a string"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun direnv-set-env-var (pair)
  "Sets an environment variable. Expects a pair of (VARNAME . VALUE)"
  (setenv (car pair) (cdr pair)))

(defun direnv-export-variables ()
  "Reads a .envrc file in the Projectile project root, and sets
environment variables for any defined exports"
  (interactive)
  (if (projectile-project-p)
      (let ((envrc (concat (projectile-project-root) ".envrc")))
        (if (file-exists-p envrc)
            (let* ((contents (direnv-read-file-as-string envrc))
                   (lines (s-split "\n" contents))
                   (exports (-filter (lambda (l) (s-starts-with? "export" l)) lines))
                   (envvars (-map (lambda (e) (direnv-parse-export e)) exports)))
              (-each envvars #'direnv-set-env-var))
          (message "No .envrc found in root of project %s" (projectile-project-name))))
    (message "No project is active")))

(provide 'direnv)
