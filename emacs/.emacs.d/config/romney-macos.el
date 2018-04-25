;;; romney-macos.el --- OSX specifics

;;; Commentary:
;;

;;; Code:

(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (autoload 'vkill "vkill" nil t)
  (global-set-key (kbd "C-x p") 'vkill)

  ;; prevent emacs error due to macos' ls not supporting long args...
  ;; ls does not support --dired; see ‘dired-use-ls-dired’
  ;; requires "brew install coreutils"
  (let ((ls-program "/usr/local/bin/gls"))
    (when (file-exists-p ls-program)
      (setq insert-directory-program ls-program))))

(setq mac-option-modifier  'control
      mac-command-modifier 'meta)

(provide 'romney-macos)
;;; romney-macos.el ends here
