;; rejuvyesh's Emacs config

;; Set User credentials
(setq user-full-name    "rejuvyesh")
(setq user-mail-address "mail@rejuvyesh.com")

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load "~/.emacs.d/init/setup-load-path.el")

;; init `setup.el' by @zk-phi for loads of optimization for init startup
(require 'setup)
(setq setup-environ-warning-alist '((emacs-version)))
(setup-initialize)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; and load custom-file
(setup-include "custom")

;; helpers
(setup "setup-helpers")

;; theme , fonts etc
(setup "setup-look")

;; editing etc
(setup "setup-editing")

;; isearch etc
(setup "setup-isearch")

;; auto-completion, yasnippets etc
(setup "setup-autocomplete")

;; modes etc
(setup "setup-major-modes")

;; use automatic file headers
(setup "setup-auto-insert")

;; others
(setup "setup-misc")
