;;; phi-search-mc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (phi-search-from-isearch-mc/setup-keys phi-search-from-isearch-mc/mark-all
;;;;;;  phi-search-from-isearch-mc/mark-previous phi-search-from-isearch-mc/mark-next
;;;;;;  phi-search-from-isearch phi-search-mc/setup-keys phi-search-mc/mark-all
;;;;;;  phi-search-mc/mark-previous phi-search-mc/mark-next phi-search-mc/mark-here)
;;;;;;  "phi-search-mc" "phi-search-mc.el" (21419 11923 602935 685000))
;;; Generated autoloads from phi-search-mc.el

(autoload 'phi-search-mc/mark-here "phi-search-mc" "\
Mark the current match as fake cursor.

With an optional argument, mark the beginning of the match instead of the end.

\(fn &optional ARG)" t nil)

(autoload 'phi-search-mc/mark-next "phi-search-mc" "\
Mark the current match as fake cursor and search next item.

With an optional number argument, marking repeats as many times
as the absolute value of the number.  If a negative argument is
given, the beginning of the match is marked instead of the end.

\(fn N)" t nil)

(autoload 'phi-search-mc/mark-previous "phi-search-mc" "\
Mark the current match as fake cursor and search previous item.

With an optional number argument, marking repeats as many times
as the absolute value of the number.  If a negative argument is
given, the beginning of the match is marked instead of the end.

\(fn N)" t nil)

(autoload 'phi-search-mc/mark-all "phi-search-mc" "\
Mark all matches as fake cursors.

\(fn)" t nil)

(autoload 'phi-search-mc/setup-keys "phi-search-mc" "\


\(fn)" nil nil)

(autoload 'phi-search-from-isearch "phi-search-mc" "\
Switch to phi-search inheriting the current isearch query.
Currently whitespace characters are taken literally, ignoring
`isearch-lax-whitespace' or `isearch-regexp-lax-whitespace'.

\(fn)" t nil)

(autoload 'phi-search-from-isearch-mc/mark-next "phi-search-mc" "\
Switch to phi-search, mark the current isearch match and search next match.

\(fn ARG)" t nil)

(autoload 'phi-search-from-isearch-mc/mark-previous "phi-search-mc" "\
Switch to phi-search, mark the current isearch match and search previous match.

\(fn ARG)" t nil)

(autoload 'phi-search-from-isearch-mc/mark-all "phi-search-mc" "\
Switch to phi-search and mark all isearch matches.

\(fn)" t nil)

(autoload 'phi-search-from-isearch-mc/setup-keys "phi-search-mc" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("phi-search-mc-pkg.el") (21419 11923 726708
;;;;;;  8000))

;;;***

(provide 'phi-search-mc-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; phi-search-mc-autoloads.el ends here
