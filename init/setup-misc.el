;; a bit more relaxed garbage collection
(setq gc-cons-threshold 20000000)

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; make zsh aliases work
(setq shell-command-switch "-lc")

;; don't keep asking me about utf8
(setq safe-local-variable-values '(
                                   (encoding . utf-8)
                                   (truncate-lines 1)
                                   (hl-line-mode 1)
                                   ))

;; make sure we always know what's happening when eval-ing things
(setq eval-expression-print-level nil)


(provide 'setup-misc)
