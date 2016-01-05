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

;; (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
;; convenience functions for loading libraries
(if (fboundp 'with-eval-after-load)
    (defalias 'load-after 'with-eval-after-load)
  (defmacro load-after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defmacro load-lazy (triggers file &rest body)
  "Load FEATURE when calling any of TRIGGERS. When loaded, eval BODY."
  (declare (indent defun))
  (let ((triggers (eval triggers)))
    `(progn
       ,@(mapcar (lambda (trigger)
                   `(autoload ',trigger ,file nil t))
                 triggers)
       (load-after ,file
                   (progn ,@body)))))

(eval-after-load "lisp-mode"
  '(font-lock-add-keywords
    'emacs-lisp-mode
    '(("(\\(load\\(?:-\\(?:after\\|lazy\\)\\)?\\)\\_>"
       1 font-lock-keyword-face)
      )))



(provide 'setup-load-path)
