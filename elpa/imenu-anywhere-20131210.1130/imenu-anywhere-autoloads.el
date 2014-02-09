;;; imenu-anywhere-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-imenu-anywhere imenu-anywhere) "imenu-anywhere"
;;;;;;  "imenu-anywhere.el" (21210 243 57357 500000))
;;; Generated autoloads from imenu-anywhere.el

(autoload 'imenu-anywhere "imenu-anywhere" "\
Switch to a buffer-local tag from Imenu via Ido.

\(fn &optional MODES)" t nil)

(defalias 'ido-imenu-anywhere 'imenu-anywhere)

(autoload 'helm-imenu-anywhere "imenu-anywhere" "\
`helm' source for `imenu-anywhere'.
Sorting is in increasing order of length of imenu symbols. The
pyramidal view allows distinguishing different buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("imenu-anywhere-pkg.el") (21210 243 143423
;;;;;;  215000))

;;;***

(provide 'imenu-anywhere-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; imenu-anywhere-autoloads.el ends here
