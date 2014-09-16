;; `local' stores manually maintained packages

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/local/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path))))
;; init has the startup scripts
(setq load-path (cons (expand-file-name "~/.emacs.d/init") load-path))

;; manual paths
(add-to-list 'load-path "~/.emacs.d/themes") ; themes

;; package-repositories
;; no need for others when you have melpa
;; TODO look into qelpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; some generic aliases that make elisp less painful (from common lisp)
(require 'cl-lib)
(defalias 'first 'cl-first)
(defalias 'second 'cl-second)

(defun pretty-load? ()
  "load stuff like themes that are only meaningful in window system?"
  (or (display-graphic-p)
      (daemonp)))

(provide 'load-path)
