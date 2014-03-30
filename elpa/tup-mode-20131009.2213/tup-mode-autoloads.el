;;; tup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tup-mode) "tup-mode" "tup-mode.el" (21304 4051
;;;;;;  246398 292000))
;;; Generated autoloads from tup-mode.el

(autoload 'tup-mode "tup-mode" "\
Major mode for editing tupfiles for the Tup build system.

\\{tup-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tup$" . tup-mode))

(add-to-list 'auto-mode-alist '("Tupfile" . tup-mode))

(add-to-list 'auto-mode-alist '("tup.config" . tup-mode))

;;;***

;;;### (autoloads nil nil ("tup-mode-pkg.el") (21304 4051 353647
;;;;;;  879000))

;;;***

(provide 'tup-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tup-mode-autoloads.el ends here
