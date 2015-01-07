;;; local packages
;; elpa package-repositories
(setup "package"

  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (defun package-disabled-packages ()
    (let (disabled-packages)
      (dolist (package package-alist)
        (let ((package-name (car package)))
          (unless (memq package-name package-activated-list)
            (add-to-list 'disabled-packages package-name))))
      disabled-packages))
  (defun package-delete-all-disabled ()
    (dolist (package (package-disabled-packages))
      (let ((version (elt (cdr (assoc package package-alist)) 0)))
        (package-delete
         (symbol-name package)
         (package-version-join version))))
    (dolist (package package-obsolete-alist)
      (dolist (version (cdr package))
        (package-delete
         (symbol-name (car package))
         (package-version-join (car version))))))
  (defun package-dependencies ()
    (let (deps)
      (dolist (package package-alist)
        (dolist (dep (elt (cdr package) 1))
          (add-to-list 'deps (car dep))))
      (sort deps 'string<)))
  (defun package-no-dependencies ()
    (let ((deps (package-dependencies))
          no-deps)
      (dolist (package package-alist)
        (let ((package-name (car package)))
          (unless (memq package-name deps)
            (add-to-list 'no-deps package-name))))
      (sort no-deps 'string<)))
  (setq package-load-list '(all))
  (package-initialize))
(provide 'setup-packages)
;;; setup-packages.el ends here
