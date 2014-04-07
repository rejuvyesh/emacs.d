;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Guide Key
(setup "guide-key"
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +" "C-x c"))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

;; make zsh aliases work
(setq shell-command-switch "-lc")

(provide 'setup-misc)
