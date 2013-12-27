;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (enh-ruby-mode) "Enhanced-Ruby-Mode/enh-ruby-mode"
;;;;;;  "Enhanced-Ruby-Mode/enh-ruby-mode.el" (21159 16342 566883
;;;;;;  137000))
;;; Generated autoloads from Enhanced-Ruby-Mode/enh-ruby-mode.el

(autoload 'enh-ruby-mode "Enhanced-Ruby-Mode/enh-ruby-mode" "\
Enhanced Major mode for editing Ruby code.

\\{enh-ruby-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))

(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))

;;;***

;;;### (autoloads (turn-off-completing-help-mode turn-on-completing-help-mode
;;;;;;  completing-help-mode) "completing-help/completing-help" "completing-help/completing-help.el"
;;;;;;  (21159 16342 566883 137000))
;;; Generated autoloads from completing-help/completing-help.el

(autoload 'completing-help-mode "completing-help/completing-help" "\
Toggle a facility to display information on completions.
With ARG, turn the mode on if ARG is positive, off otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-completing-help-mode "completing-help/completing-help" "\
Turn on a facility to display information on completions.

\(fn)" t nil)

(autoload 'turn-off-completing-help-mode "completing-help/completing-help" "\
Turn off a facility to display information on completions.

\(fn)" t nil)

;;;***

;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-packages-of-type el-get-update-all
;;;;;;  el-get-version) "el-get/el-get" "el-get/el-get.el" (21159
;;;;;;  16353 556500 756000))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get/el-get-list-packages"
;;;;;;  "el-get/el-get-list-packages.el" (21159 16353 613165 451000))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (virtualenv-minor-mode virtualenv-deactivate virtualenv-workon)
;;;;;;  "virtualenv/virtualenv" "virtualenv/virtualenv.el" (21159
;;;;;;  16341 56935 681000))
;;; Generated autoloads from virtualenv/virtualenv.el

(autoload 'virtualenv-workon "virtualenv/virtualenv" "\
Activate a virtual environment for python.
Optional argument ENV if non-nil, either use the string given as
the virtual environment or if not a string then query the user.

\(fn &optional ENV)" t nil)

(autoload 'virtualenv-deactivate "virtualenv/virtualenv" "\


\(fn)" t nil)

(autoload 'virtualenv-minor-mode "virtualenv/virtualenv" "\
Toggle Virtualenv minor mode on or off.
With a prefix argument ARG, enable Virtualenv minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{virtualenv-minor-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (webjump++) "webjump++/webjumps++" "webjump++/webjumps++.el"
;;;;;;  (21159 16354 996450 657000))
;;; Generated autoloads from webjump++/webjumps++.el

(autoload 'webjump++ "webjump++/webjumps++" "\
Open a browser using `browse-url' for JUMP and search QUERY.

Prompt for JUMP if not defined.

If QUERY is not defined, try to search in order:
  - text in active region,
  - `word-at-point'
  - read from minibuffer.

QUERY is read from minibuffer if called with
`universal-argument' (C-u) or UARGS is defined.

\(fn &optional UARG JUMP QUERY)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete/auto-complete-config.el"
;;;;;;  "auto-complete/auto-complete-pkg.el" "el-get/el-get-autoloads.el"
;;;;;;  "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "mu4e/mu4e/mu4e-about.el"
;;;;;;  "mu4e/mu4e/mu4e-actions.el" "mu4e/mu4e/mu4e-contrib.el" "mu4e/mu4e/mu4e-draft.el"
;;;;;;  "mu4e/mu4e/mu4e-headers.el" "mu4e/mu4e/mu4e-lists.el" "mu4e/mu4e/mu4e-main.el"
;;;;;;  "mu4e/mu4e/mu4e-mark.el" "mu4e/mu4e/mu4e-message.el" "mu4e/mu4e/mu4e-meta.el"
;;;;;;  "mu4e/mu4e/mu4e-proc.el" "mu4e/mu4e/mu4e-utils.el" "mu4e/mu4e/mu4e-vars.el"
;;;;;;  "mu4e/mu4e/mu4e-view.el" "mu4e/mu4e/org-mu4e.el" "smartparens/smartparens-config.el"
;;;;;;  "smartparens/smartparens-html.el" "smartparens/smartparens-latex.el"
;;;;;;  "smartparens/smartparens-lua.el" "smartparens/smartparens-pkg.el"
;;;;;;  "smartparens/smartparens-ruby.el") (21173 39515 136247 946000))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
