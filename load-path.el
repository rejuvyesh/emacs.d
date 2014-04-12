;; local stores manually maintained packages
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

;; some generic aliases that make elisp less painful
(require 'cl-lib)
(defalias 'first 'cl-first)
(defalias 'second 'cl-second)

(defun pretty-load? ()
  "load stuff like themes that are only meaningful in window system?"
  (or (display-graphic-p)
      (daemonp)))

(provide 'load-path)
