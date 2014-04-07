;; rejuvyesh's Emacs config

;;; Code:

(setq user-full-name "Jayesh Kumar Gupta"
      user-mail-address "mail@rejuvyesh.com")

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load "~/.emacs.d/load-path.el")

;; init setup
(require 'setup)
(setq setup-environ-warning-alist '((emacs-version)))
(setup-initialize)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;(load custom-file)
(setup-include "custom")

;; some generic aliases that make elisp less painful
(defalias 'first  'cl-first)
(defalias 'second 'cl-second)

;; theme etc
(setup "setup-look")

;; editing etc
(setup "setup-editing")

;; isearch etc
(setup "setup-isearch")

;; auto-completion etc
(setup "setup-autocomplete")

;; use automatic file headers
(setup "setup-auto-insert")

;; modes etc
(setup "setup-major-modes")

;; others
(setup "setup-misc")
