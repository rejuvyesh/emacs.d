;;; robe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (robe-mode) "robe" "robe.el" (21189 33900 803059
;;;;;;  8000))
;;; Generated autoloads from robe.el

(autoload 'robe-mode "robe" "\
Improved navigation for Ruby.

The following commands are available:

\\{robe-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (robe-ac-setup robe-ac-available) "robe-ac" "robe-ac.el"
;;;;;;  (21189 33900 836391 181000))
;;; Generated autoloads from robe-ac.el

(autoload 'robe-ac-available "robe-ac" "\
Return t if `robe-mode' completions are available, otherwise nil.

\(fn)" nil nil)

(autoload 'robe-ac-setup "robe-ac" "\


\(fn)" nil nil)

(defconst ac-source-robe '((available . robe-ac-available) (candidates . robe-ac-candidates) (document . robe-ac-doc) (symbol . "r")) "\
`auto-complete' completion source for Ruby using `robe-mode'.")

;;;***

;;;### (autoloads (company-robe) "robe-company" "robe-company.el"
;;;;;;  (21189 33900 869723 355000))
;;; Generated autoloads from robe-company.el

(autoload 'company-robe "robe-company" "\
A `company-mode' completion back-end for `robe-mode'.

\(fn COMMAND &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("robe-pkg.el") (21189 33900 942342 420000))

;;;***

(provide 'robe-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; robe-autoloads.el ends here
