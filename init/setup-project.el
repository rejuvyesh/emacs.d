(use-package projectile-ripgrep
  :commands (projectile-ripgrep))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  :config
  (projectile-global-mode)
  (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-completion-system 'helm)
  (setq projectile-known-projects-file (emacs-d "cache/projectile-bookmarks.eld"))
  (if (executable-find "ag")
      (define-key projectile-command-map (kbd "s") 'projectile-ag))
  (if (executable-find "rg")
      (define-key projectile-command-map (kbd "s") 'projectile-ripgrep))
  )

(provide 'setup-project)
