;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; make zsh aliases work
(setq shell-command-switch "-lc")

(provide 'setup-misc)
