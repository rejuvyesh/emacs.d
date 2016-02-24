;; Set User credentials
(setq user-full-name    "rejuvyesh")
(setq user-mail-address "mail@rejuvyesh.com")

;; load path
(defun emacs-d (path)
  (let ((user-dir
         (cond ((boundp 'user-init-dir) user-init-dir)  ; check if user-init-dir is defined
               ((boundp 'user-emacs-directory) user-emacs-directory) ; else if user-emacs-directory is defined
               (t "~/.emacs.d/")))) ; else default
    (concat user-dir path)))

;; load path (the only hard-coded path, so we can use the file in external scripts without duplicating where load-paths are defined)
(load (emacs-d "init/setup-load-path.el"))

;; packages
(require 'setup-packages)

;; helpers
(require 'setup-helpers)

;; theme , fonts etc
(require 'setup-look)

;; editing etc
(require 'setup-helm)
(require 'setup-editing)
(require 'setup-folding)

;; isearch etc
(require 'setup-search)

;; auto-completion, yasnippets etc
(require 'setup-autocomplete)

;; modes etc
(require 'setup-major-modes)

;; project management
(require 'setup-project)

;; use automatic file headers
(require 'setup-auto-insert)

;; mail
(require 'setup-mail)

;; others
(require 'setup-misc)
