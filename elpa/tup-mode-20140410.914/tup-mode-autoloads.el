;;; tup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tup-mode) "tup-mode" "tup-mode.el" (21318 57261
;;;;;;  238129 102000))
;;; Generated autoloads from tup-mode.el

(autoload 'tup-mode "tup-mode" "\
Major mode for editing tupfiles for the Tup build system.

\\{tup-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tup$" . tup-mode))

(add-to-list 'auto-mode-alist '("Tupfile" . tup-mode))

(add-to-list 'auto-mode-alist '("tup.config" . tup-mode))

;;;***

;;;### (autoloads nil nil ("tup-mode-pkg.el") (21318 57261 371646
;;;;;;  20000))

;;;***

(provide 'tup-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tup-mode-autoloads.el ends here
