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
  (define-key projectile-command-map (kbd "s") 'projectile-ag))

(provide 'setup-project)
