;;; auto-yasnippet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (aya-create aya-create-one-line) "auto-yasnippet"
;;;;;;  "auto-yasnippet.el" (21502 8070 81664 358000))
;;; Generated autoloads from auto-yasnippet.el

(autoload 'aya-create-one-line "auto-yasnippet" "\
A simplistic `aya-create' to create only one mirror.
You can still have as many instances of this mirror as you want.
It's less flexible than `aya-create', but faster.
It uses a different marker, which is `aya-marker-one-line'.
You can use it to quickly generate one-liners such as
menu.add_item(spamspamspam, \"spamspamspam\")

\(fn)" t nil)

(autoload 'aya-create "auto-yasnippet" "\
Works on either the current line, or, if `mark-active', the current region.
Removes `aya-marker' prefixes,
writes the corresponding snippet to `aya-current',
with words prefixed by `aya-marker' as fields, and mirrors properly set up.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-yasnippet-pkg.el") (21502 8070 172321
;;;;;;  532000))

;;;***

(provide 'auto-yasnippet-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-yasnippet-autoloads.el ends here
