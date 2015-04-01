;;; phi-search-mc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "phi-search-mc" "phi-search-mc.el" (21736 12470
;;;;;;  936478 943000))
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

\(fn &optional INIT-FUNC)" t nil)

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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; phi-search-mc-autoloads.el ends here
