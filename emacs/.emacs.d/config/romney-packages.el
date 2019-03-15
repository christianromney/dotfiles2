;;; romney-packages.el --- Default package selection

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'cl))
(require 'package)

(setq package-archives          '(("melpa-stable" . "http://stable.melpa.org/packages/")
                                  ("melpa" . "https://melpa.org/packages/")
                                  ("gnu" . "http://elpa.gnu.org/packages/")
                                  ("org" . "http://orgmode.org/elpa/"))
      package-enable-at-startup nil
      package-user-dir          (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

(defun romney-install-use-package ()
  "Installs use-package for package management."
  (unless (package-installed-p 'use-package)
    (message "Refreshing package database...")
    (package-refresh-contents)
    (message "done.")
    (package-install 'use-package)))

(romney-install-use-package)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package elpa-mirror
  :ensure t
  :init
  (setq elpamr-default-output-directory "~/.emacs.d/elpa-mirror"))

(global-set-key (kbd "M-<f2>") #'package-list-packages)

(provide 'romney-packages)
;;; romney-packages.el ends here
