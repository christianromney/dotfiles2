;;; personal/prelude-rails --- Ruby and Rails Configuration
;;;
;;; Commentary:
;;; smartparens + web-mode is a disaster for ruby-templates
;;; 
;;; Code:
;;;
(use-package rinari :defer t)

(use-package bundler :defer t)

(use-package robe
  :defer t
  :init
  (progn (add-hook 'ruby-mode-hook 'robe-mode)
         (push 'company-robe company-backends)))

(use-package flymake-ruby
  :config
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline t)
  (global-rbenv-mode 1))

(use-package rspec-mode
  :defer t
  :init
  (defun personal/rspec-verify-single ()
    "Runs the specified example at the point of the current buffer."
    (interactive)
    (rspec-run-single-file
     (concat
      (rspec-spec-file-for (buffer-file-name))
      ":"
      (save-restriction
        (widen)
        (number-to-string (line-number-at-pos))))
     (rspec-core-options)))
  :config
  (progn
    (setq rspec-command-options "--fail-fast --format documentation")
    (bind-key "C-c , ," 'rspec-rerun rspec-mode-map)
    (fset 'rspec-verify-single 'personal/rspec-verify-single)))

(provide 'personal/prelude-rails)
;;; prelude-rails ends here
