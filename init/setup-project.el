(defun projectile-ripgrep (regexp)
  "Run a Ripgrep search with `REGEXP' rooted at the current projectile project root."
  (interactive
   (list
    (read-from-minibuffer "Ripgrep search for: " (thing-at-point 'symbol))))
  (if (fboundp 'projectile-project-root)
      (ripgrep-regexp regexp
                      (projectile-project-root)
                      (mapcar (lambda (val) (concat "--glob !" val))
                              (append projectile-globally-ignored-files
                                      projectile-globally-ignored-directories))
                      )
    (error "Projectile is not available")))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  (projectile-global-mode)
  :config
  (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-completion-system 'helm)
  (setq projectile-known-projects-file (emacs-d "cache/projectile-bookmarks.eld"))
  (if (executable-find "ag")
      (define-key projectile-command-map (kbd "s") 'projectile-ag))
  (if (executable-find "rg")
      (define-key projectile-command-map (kbd "s") 'projectile-ripgrep))
  )

(provide 'setup-project)
