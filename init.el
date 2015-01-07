;; rejuvyesh's Emacs config

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

;; init `setup.el' by @zk-phi for loads of optimization for init startup
(require 'setup)
(setq setup-environ-warning-alist '((emacs-version)))
(setup-initialize)

;; packages
(setup "setup-packages")

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
