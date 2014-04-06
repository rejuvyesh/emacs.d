;; rejuvyesh's Emacs config

;;; Code:

(setq user-full-name "Jayesh Kumar Gupta"
      user-mail-address "mail@rejuvyesh.com")

;; site-lisp stores manually maintained packages
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/local/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))

;; package-repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; init setup
(require 'cl-lib)
(require 'setup)
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
  (setup-after "anzu-mode"(diminish 'anzu-mode))
  (setup-after "auto-complete"(diminish 'auto-complete-mode "↝"))
  (setup-after "auto-revert-mode" (diminish 'auto-revert-mode))
  (setup-after "eldoc" (diminish 'eldoc-mode))
  (setup-after "fic-mode" (diminish 'fic-mode))
  (setup-after "guide-key" (diminish 'guide-key-mode))
  (setup-after "haskell-doc" (diminish 'haskell-doc-mode))
  (setup-after "haskell-indentation" (diminish 'haskell-indentation-mode))
  (setup-after "highlight-parentheses" (diminish 'highlight-parentheses-mode))
  (setup-after "hs-minor-mode" (diminish 'hs-minor-mode))
  (setup-after "smartparens-autoloads" (diminish 'smartparens-mode))
  (setup-after "undo-tree" (diminish 'undo-tree-mode "↺"))
  (setup-after "visual-line-mode" (diminish 'visual-line-mode))
  (setup-after "volatile-highlights" (diminish 'volatile-highlights-mode))
  (setup-after "whole-line-or-region" (diminish 'whole-line-or-region-mode))
  (setup-after "yasnippet" (diminish 'yas-minor-mode)))
