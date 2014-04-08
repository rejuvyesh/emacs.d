;; major modes

;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; don't let them steal you keys
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

;; Flycheck for code linting
(setup "flycheck"
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (unbreak-stupid-map flycheck-mode-map)
  (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c C-p") 'flycheck-previous-error))

;; C coding style
(setq c-default-style "linux"
      c-basic-offset tab-width
      c-block-comment-prefix "* ")
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)
(setup-lazy '(c-turn-on-eldoc-mode) "c-eldoc"
  (setq c-eldoc-buffer-regenerate-time 15))
(setup-after "cc-mode"
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode))

(setup-after "auto-complete-config"
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
  (defun ac-cc-mode-setup ()
    (setq ac-clang-complete-executable "~/.emacs.d/el-get/emacs-clang-complete-async/clang-complete")
    (setq ac-sources '(ac-source-clang-async))
    (ac-clang-launch-completion-process)
    )
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup))

;; load ESS for R
;; (setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode)
(setup-lazy '(R R-mode julia-mode) "ess-site"
  (setq inferior-julia-program-name "~/dev/julia/julia/usr/bin/julia-basic")))

;; auctex
(setup-lazy '(latex-mode LaTeX-mode tex-mode TeX-mode) "latex"
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex)      ; use xelatex by default
  (setq TeX-view-program-selection '((output-pdf "zathura")))
  (add-to-list 'ac-modes 'LaTeX-mode))   ; make auto-complete aware of `latex-mode`

;;(setup-lazy '(org-mode markdown-mode latex-mode) "ac-math"
(setup "ac-math"
  (defvar ac-source-math-latex-everywhere
    '((candidates . ac-math-symbols-latex)
      (prefix . "\\\\\\(.*\\)")
      (action . ac-math-action-latex)
      (symbol . "l"))))
(setup-after "ac-math"
  (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
    (setq ac-sources
          (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                  ac-sources)))
  (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
  (add-hook 'org-mode-hook
            (lambda()
              (add-to-list 'ac-sources 'ac-source-math-unicode)
              (add-to-list 'ac-sources 'ac-source-math-latex-everywhere)))
  (add-hook 'markdown-mode-hook
            (lambda()
              (add-to-list 'ac-sources 'ac-source-math-latex-everywhere)))
  )

;; markdown
(setup-lazy '(markdown-mode) "markdown-mode"
  (setq markdown-command "pandoc --smart -f markdown -t html")
  ;; add pandoc hook
  (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  )
(add-to-list 'auto-mode-alist '("\\.pdc$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\bREADME$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page$" . markdown-mode))

;; yaml
(setup-lazy '(yaml-mode) "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; org-mode
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; loaded so that we can diminish it later
(setup-lazy '(org-mode) "org-indent"
  ;; proper indentation / folding
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))))
;; tag column
(setq org-tags-column -70)
;; dependencies
(setq org-enforce-todo-dependencies t)
;; make clock history persistent
(setq org-clock-persist 'history)

;; notes file
(setq org-default-notes-file "~/Documents/spoiler/notes.org")
(define-key global-map "\C-cC" 'org-capture)
(setq org-capture-templates
      '(("l" "Link" plain (file "~/Documents/spoiler/links.org")
         "- %?\n %x\n")
        ("n" "note" entry (file "~/Documents/spoiler/notes.org")
         "* %? %^g\n%U\n%a\n")
        ("q" "quote" entry (file "~/Documents/spoiler/quotes.org")
         "* %? \n%x\n%a\n")))
;; Todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))
;; priorities
(setq org-default-priority 67) ;C
;; spoiler files
(defadvice org-todo-list (before org-todo-list-reload activate compile)
  "Scan for org files whenever todo list is loaded."
                                        ; 'find' is faster and has better control than lisp
  (setq org-agenda-files (mapcar 'abbreviate-file-name (split-string
                                                        (shell-command-to-string "find ~/Documents/spoiler -type f -name \"*.org\" | sort")
                                                        "\n"))))
;; code block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C          . t)
   (R          . t)
   (matlab     . t)
   (sh         . t)
   (ruby       . t)
   (python     . t)
   (haskell    . t)))
(add-to-list 'org-src-lang-modes '("c" . c))
(add-to-list 'org-src-lang-modes '("r" . ess-mode))
(add-to-list 'org-src-lang-modes '("h" . haskell))
(add-to-list 'org-src-lang-modes '("s" . sh))
(add-to-list 'org-src-lang-modes '("p" . python))
(add-to-list 'org-src-lang-modes '("ruby" . enh-ruby))
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(org-defkey org-mode-map "\C-c\C-t" (lambda () (interactive) (org-todo "TODO")))
(org-defkey org-mode-map "\C-c\C-w" (lambda () (interactive) (org-todo "WAITING")))
(org-defkey org-mode-map "\C-c\C-d" (lambda () (interactive) (org-todo "DONE")))
;; shortcut for C-u C-c C-l
(defun org-insert-file-link () (interactive) (org-insert-link '(4)))
(org-defkey org-mode-map "\C-cl" 'org-store-link)

;; some templates
(setcdr (assoc "c" org-structure-template-alist)
        '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
(add-to-list 'org-structure-template-alist
             '("r"
               "#+BEGIN_SRC ruby\n?\n#+END_SRC"
               "<src lang=\"ruby\">\n\n</src>"))


;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; python 

(setup-lazy '(python-mode) "elpy"
  (elpy-enable)
  (elpy-use-ipython)
  (elpy-clean-modeline))

;; haskell mode
(setup-lazy '(haskell-mode) "haskell-mode")
(setup-after "haskell-mode"
  (setup "haskell-doc"
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode))
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (setup "haskell-indentation"
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
  (setup "inf-haskell"
    (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))
  (define-key haskell-mode-map (kbd "C-c ?") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info))

;; ruby ;;
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(setup-lazy '(ruby-mode enh-ruby-mode) "enh-ruby-mode"
  (setq enh-ruby-program "~/.rbenv/shims/ruby")
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
  ;; replace normal ruby mode
  (defalias 'ruby-mode 'enh-ruby-mode)
  ;; better colors for warnings
  (defface erm-syn-warnline
    '((t (:underline "orange")))
    "Face used for marking warning lines."
    :group 'enh-ruby)
  (defface erm-syn-errline
    '((t (:underline "pink")))
    "Face used for marking error lines."
    :group 'enh-ruby)
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren nil))

;; Rake files are Ruby, too
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))

(setup-after "enh-ruby-mode"
  ;; misc stuff
  ;; ri documentation tool
  (setup "yari"
    (define-key enh-ruby-mode-map (kbd "C-c ?") 'yari)) 
  (setup "ruby-block" ; show what block an end belongs to
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  ;; erb
  (setup-lazy '(rhtml-mode)"rhtml-mode")
  (add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode)))

;; Sometimes you have to
(setup-lazy '(php-mode) "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; javascript
(setup-lazy '(js2-mode) "js2-mode")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; lua
(setup-lazy '(lua-mode) "lua-mode"
  (setq lua-indent-level 2))

;; (s)css
(setup-lazy '(scss-mode) "scss-mode"
  (setq scss-compile-at-save nil))
(setup-lazy '(css-mode) "css-mode"
  (setq css-indent-level 2))

;; eldoc, ie function signatures in the minibuffer
(setup-lazy '(turn-on-eldoc-mode) "eldoc"
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))
(setup-after "lisp-mode"
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; emacs-lisp
(setup "bytecomp"
  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'byte-compile-current-buffer))

;; go
(setup-lazy '(go-mode) "go-mode"
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (define-key go-mode-map (kbd "M-t") 'godef-jump)
  (define-key go-mode-map (kbd "M-T") 'godef-jump-other-window)
  (setup "go-eldoc"
    (add-hook 'go-mode-hook 'go-eldoc-setup)))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; crontab
(setup-lazy '(crontab-mode) "crontab-mode")
(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))

;; mark stuff like FIXME
(setup-lazy '(fic-mode) "fic-mode")
(add-hook 'prog-mode-hook 'fic-mode)
;; misbehaving modes
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook 'fic-mode)

;; csv
(setup-lazy '(csv-mode) "csv-mode"
  (setup "csv-nav")
  (setq csv-separators '("," ";" "|" " ")))
(add-to-list 'auto-mode-alist 'csv-mode "\\.[Cc][Ss][Vv]\\'")

;; json
(setup-lazy '(json-mode) "json-mode")
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; matlab
(setup-lazy '(matlab-mode) "matlab-load"
  (add-hook 'matlab-mode
            (lambda ()
              (auto-complete-mode 1)
              )))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

;; dired
(setup-lazy '(dired-jump) "dired"
  ;; move files between split pans
  (setq dired-dwim-target t))
(setup-after "dired" 
  (setup "wdired")
  (setup "dired-details")
  (setup "dired-details+")
  
  ;; reload dired after making changes
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))
  (global-set-key (kbd "C-c C-j") 'dired-jump)
  (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "<insert>") 'dired-mark)
  ;; C-a goes to filename
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
  ;; M-up goes to first file
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
  ;; M-down goes to last file
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; mutt
;; mail support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook (lambda () (setq fill-column 72)))


;; smart-compile
(setup-lazy '(smart-compile) "smart-compile"
  (setq smart-compile-alist
        '( (emacs-lisp-mode  . (emacs-lisp-byte-compile))
           (html-mode        . (browse-url-of-buffer))
           (nxhtml-mode      . (browse-url-of-buffer))
           (html-helper-mode . (browse-url-of-buffer))
           (octave-mode      . (run-octave))
           (c-mode           . "gcc -c99 -pedantic -Wall -W -Wextra -Wunreachable-code %f")
           (java-mode        . "javac -Xlint:all -encoding UTF-8 %f")
           (haskell-mode     . "ghc -Wall -fwarn-missing-import-lists %f") )))
(global-set-key (kbd "C-S-c") '("smart-compile" smart-compile compile))

(provide 'setup-major-modes)
