;; rejuvyesh's Emacs config

;;; Code:

(setq user-full-name "Jayesh Kumar Gupta"
      user-mail-address "mail@rejuvyesh.com")

;; site-lisp stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; package-repositories
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; init setup
(require 'cl)
(require 'setup)
(setup-initialize)

;; theme etc
(setup-include "~/.emacs.d/setup-look.el")

;; editing etc
(setup-include "~/.emacs.d/setup-editing.el")

;; isearch etc
(setup-include "~/.emacs.d/setup-isearch.el")

;; auto-completion etc
(setup-include "~/.emacs.d/setup-autocomplete.el")

;; use automatic file headers
(setup-include "~/.emacs.d/setup-auto-insert.el")

;; modes etc
(setup-include "~/.emacs.d/setup-major-modes.el")

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

;; diminish
(setup "diminish"
  (diminish 'anzu-mode)
  (diminish 'auto-complete-mode "↝")
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'fic-mode)
  (diminish 'guide-key-mode)
  (diminish 'haskell-doc-mode)
  (diminish 'haskell-indentation-mode)
  (diminish 'highlight-parentheses-mode)
  (diminish 'hs-minor-mode)
  (diminish 'smartparens-mode)
  (diminish 'undo-tree-mode "↺")
  (diminish 'visual-line-mode)
  (diminish 'volatile-highlights-mode)
  (diminish 'whole-line-or-region-mode)
  (diminish 'yas-minor-mode))
