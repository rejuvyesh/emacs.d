;;; phi-grep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "phi-grep" "phi-grep.el" (22153 60636 889155
;;;;;;  613000))
;;; Generated autoloads from phi-grep.el

(autoload 'phi-grep-in-file "phi-grep" "\
phi-grep in a single file.

\(fn FNAME REGEXP)" t nil)

(autoload 'phi-grep-in-directory "phi-grep" "\
phi-grep in all files recursively in a directory.

\(fn TREE REGEXP &optional ONLY)" t nil)

(autoload 'phi-grep-dired-in-dir-at-point "phi-grep" "\
phi-grep in all files recursively in the selected directory.

\(fn REGEX &optional ONLY)" t nil)

(autoload 'phi-grep-dired-in-file-at-point "phi-grep" "\
phi-grep in the selected file.

\(fn REGEX)" t nil)

(autoload 'phi-grep-dired-in-marked-files "phi-grep" "\
phi-grep in all marked files.

\(fn REGEXP)" t nil)

(autoload 'phi-grep-dired-in-all-files "phi-grep" "\
phi-grep in all files in the current directory.

\(fn REGEXP ONLY)" t nil)

(autoload 'phi-grep-dired-dwim "phi-grep" "\
Call the phi-grep-dired command you want (Do What I Mean).

- file at point
- marked files
- directory at point (recursion)

\(fn REGEXP &optional ONLY)" t nil)

(autoload 'phi-grep-find-file-flat "phi-grep" "\


\(fn TREE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; phi-grep-autoloads.el ends here
