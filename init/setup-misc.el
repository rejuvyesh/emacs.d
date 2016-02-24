;; a bit more relaxed garbage collection
(defun my-minibuffer-setup-hook()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook()
  (setq gc-cons-threshold (* 100 1024)))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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
