;; `local' stores manually maintained packages

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir (emacs-d "local/"))
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
;; init has the startup scripts
(setq load-path (cons (expand-file-name (emacs-d "init")) load-path))

;; manual paths
(add-to-list 'load-path (emacs-d "themes")) ; themes

(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")

(provide 'setup-load-path)
