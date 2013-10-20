;;; gitignore-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gitignore-mode) "gitignore-mode" "gitignore-mode.el"
;;;;;;  (21091 45645 986071 158000))
;;; Generated autoloads from gitignore-mode.el

(autoload 'gitignore-mode "gitignore-mode" "\
A major mode for editing .gitignore files.

\(fn)" t nil)

(dolist (pattern (list (rx "/.gitignore" string-end) (rx "/.git/info/exclude" string-end) (rx "/git/ignore" string-end))) (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;;***

;;;### (autoloads nil nil ("gitignore-mode-pkg.el") (21091 45646
;;;;;;  125382 617000))

;;;***

(provide 'gitignore-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitignore-mode-autoloads.el ends here
