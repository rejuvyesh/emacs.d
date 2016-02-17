;; major modes

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-process-popup-time -1
        magit-auto-revert-mode-lighter nil
        magit-push-always-verify·nil)
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (if (or
               ;; the original should stay alive, so we can't go fullscreen
               magit-display-buffer-noselect
               ;; don't go fullscreen for certain magit buffers if current
               ;; buffer is a magit buffer (we're conforming to
               ;; `magit-display-buffer-traditional')
               (and (derived-mode-p 'magit-mode)
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))))
              ;; open buffer according to original magit rules
              (magit-display-buffer-traditional buffer)
            ;; open buffer in fullscreen
            (delete-other-windows)
            ;; make sure the window isn't dedicated, otherwise
            ;; `set-window-buffer' throws an error
            (set-window-dedicated-p nil nil)
            (set-window-buffer nil buffer)
            ;; return buffer's window
            (get-buffer-window buffer))))
  )

;; default modes
(use-package org
  :ensure t
  :mode ("\\\.org\\\'" . org-mode)
  :init
  (setq initial-major-mode 'org-mode)
  (setq org-startup-indented t)
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 2)
  (setq org-startup-folded 'content)
  :config
  (setq-default major-mode 'org-mode)
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto)))
  ;; tag column
  (setq org-tags-column -70)
  ;; dependencies
  (setq org-enforce-todo-dependencies t)
  ;; make clock history persistent
  (setq org-clock-persist 'history)
  ;; Todo states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))
  ;; priorities
  (setq org-default-priority 67) ;C
  ;; highlight math
  (setf org-highlight-latex-and-related '(latex entities))
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

  (org-defkey org-mode-map (kbd "C-c C-t") (lambda () (interactive) (org-todo "TODO")))
  (org-defkey org-mode-map (kbd "C-c C-w") (lambda () (interactive) (org-todo "WAITING")))
  (org-defkey org-mode-map (kbd "C-c C-d") (lambda () (interactive) (org-todo "DONE")))
  ;; shortcut for C-u C-c C-l
  (defun org-insert-file-link () (interactive) (org-insert-link '(4)))
  (org-defkey org-mode-map (kbd "C-c l") 'org-store-link)
  
  ;; some templates
  (setcdr (assoc "c" org-structure-template-alist)
          '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
  (add-to-list 'org-structure-template-alist
               '("r"
                 "#+BEGIN_SRC ruby\n?\n#+END_SRC"
                 "<src lang=\"ruby\">\n\n</src>"))
  (add-to-list 'org-structure-template-alist
               '("p"
                 "#+BEGIN_SRC python\n?\n#+END_SRC"
                 "<src lang=\"python\">\n\n</src>"))
  (add-to-list 'org-structure-template-alist
               '("b"
                 "#+BEGIN_SRC bib\n?\n#+END_SRC"
                 "<src lang=\"bib\">\n\n</src>"))
  
  ;; (use-package org-journal)
  ;; (setq org-journal-dir "~/Documents/spoiler/logs/")
  ;; (setq org-journal-file-format "%Y-%m-%d.org")
  ;; (global-set-key (kbd "C-c j") 'org-journal-new-entry)

  ;; (use-package org-download)
  (use-package org-pdfview
    :ensure t)
  (add-to-list 'org-file-apps '("\\.pdf\\'"                   . org-pdfview-open))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
  )


;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; don't let them steal you keys
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

;; Flycheck for code linting
(use-package flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (unbreak-stupid-map flycheck-mode-map)
  (define-key flycheck-mode-map (kbd "C-c C-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c C-p") 'flycheck-previous-error)

;; C coding style
(setq c-default-style "linux"
      c-basic-offset tab-width
      c-block-comment-prefix "* ")
(global-set-key (kbd "M-RET") 'c-indent-new-comment-line)

(load-lazy '(c-turn-on-eldoc-mode) "c-eldoc"
  (setq c-eldoc-buffer-regenerate-time 15))
(load-after 'cc-mode
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook   'c-turn-on-eldoc-mode))

;; show what function we're in
(use-package which-func)
(which-function-mode 1)

(load-after 'auto-complete-config
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

;; load ESS for R
;; (setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode)
(load-lazy '(R R-mode julia-mode) "ess-site"
  (setq inferior-julia-program-name "julia")))

;; auctex
(load-lazy '(latex-mode LaTeX-mode) "latex"
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  (setq TeX-source-correlate-method 'synctex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-newline-function 'reindent-then-newline-and-indent)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex)      ; use xelatex by default
  (setq TeX-view-program-list
        '(("zathura"
           ("zathura" (mode-io-correlate "-sync.sh")
            " "
            (mode-io-correlate "%n:1:%b ")
            "%o"))))
  (setq TeX-view-program-selection '((output-pdf "zathura")))
  (load-after 'auto-complete
      (push 'LaTeX-mode ac-modes)  ; make auto-complete aware of `latex-mode`
      )
  )

;; markdown
(load-lazy '(markdown-mode) "markdown-mode"
  (setq markdown-command "pandoc --smart -f markdown -t html")
  (setq markdown-css-path (expand-file-name "markdown.css" "~/.pandoc/css/markdown.css"))
  (setq markdown-enable-math t)
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

;; yaml
(load-lazy '(yaml-mode) "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; muflax-notes
(load-lazy '(notes-mode) "notes-mode")



;; Automatic Math preview toggle in org-mode
;; Source: http://goo.gl/WLYzxp
(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun org-latex-fragment-toggle ()
  "Toggle a latex fragment image "
  (and (eq 'org-mode major-mode)
       (let* ((el (org-element-context))
              (el-type (car el)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-latex-fragment-last
            ;; and are on a fragment now
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type))
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (= (org-element-property :begin el)
                    (org-element-property :begin org-latex-fragment-last))))
           ;; go back to last one and put image back
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; now remove current image
           (goto-char (org-element-property :begin el))
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           ;; and save new fragment
           (setq org-latex-fragment-last el))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type)))
            ;; but we were on one
            org-latex-fragment-last)
           ;; put image back on
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; unset last fragment
           (setq org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; but now we are
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type)))
           (goto-char (org-element-property :begin el))
           ;; remove image
           (let ((ov (loop for ov in org-latex-fragment-image-overlays
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))
(add-hook 'post-command-hook 'org-latex-fragment-toggle)

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
;; also revert dired
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
(setq auto-revert-verbose nil)

;; python 

(load-lazy '(python-mode) "python"
  (setq python-indent-offset 2)
  (add-hook 'python-mode-hook (lambda () (setq tab-width 2)))
  (unbreak-stupid-map python-mode-map)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-require-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  )

;; haskell mode
(load-lazy '(haskell-mode) "haskell-mode")
(load-after 'haskell-mode
  (use-package haskell-doc)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (use-package inf-haskell)
    (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-log t)
  (define-key haskell-mode-map (kbd "C-c ?") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info))

;; ruby ;;
;; replace normal ruby mode
(defalias 'ruby-mode 'enh-ruby-mode)
;; enhanced ruby mode
(load-lazy '(ruby-mode enh-ruby-mode) "enh-ruby-mode"
  (setq enh-ruby-program "~/.rbenv/shims/ruby")

  ;; we use flycheck to cover errors
  (setq enh-ruby-check-syntax nil)

  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren nil)

  (add-hook 'enh-ruby-mode-hook 'whitespace-mode))

;; Rake files are Ruby, too
(add-to-list 'interpreter-mode-alist '("ruby"        . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("\\.rake$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("Rakefile$"   . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("Gemfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("Capfile$"    . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("\\.builder$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist        '("\\.gemspec$" . enh-ruby-mode))

(load-after 'enh-ruby-mode
  ;; misc stuff
  (use-package ruby-block) ; show what block an end belongs to
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t))
  

;; lua
(load-lazy '(lua-mode) "lua-mode"
  (setq lua-indent-level 2))

;; (s)css
(load-lazy '(scss-mode) "scss-mode"
  (setq scss-compile-at-save nil))

;; web-mode
(load-lazy '(web-mode) "web-mode"
  :prepare (push (! `(,(format "\\.%s$"
                               (regexp-opt
                                '("phtml" "tpl" "php" "gsp" "jsp"
                                  "aspx" "ascx" "erb" "mustache" "djhtml"
                                  "html" "js" "jsx" "css" "xml")))
                      . web-mode)) auto-mode-alist)
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 2)

  (require-keybinds web-mode-map
    [remap comment-dwim] 'web-mode-comment-or-uncomment
    "C-c C-'" 'web-mode-element-close)

  (load-after 'auto-complete
    (load-after 'auto-complete-config
      (setq web-mode-ac-sources-alist
            '(("javascript" . (ac-source-words-in-same-mode-buffers))
              ("php" . (ac-source-words-in-same-mode-buffers))
              ("css" . (ac-source-css-property ac-source-words-in-same-mode-buffers))
              ("html" . (ac-source-words-in-same-mode-buffers))))
      (push 'web-mode ac-modes)))

  (load-after 'smart-compile
    (push '(web-mode . (browse-url-of-buffer)) smart-compile-alist))
  )

;; eldoc, ie function signatures in the minibuffer
(load-lazy '(turn-on-eldoc-mode) "eldoc"
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))
(load-after 'lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))


;; go
(load-lazy '(go-mode) "go-mode"
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (define-key go-mode-map (kbd "M-t") 'godef-jump)
  (define-key go-mode-map (kbd "M-T") 'godef-jump-other-window))
(load-after 'go-mode
  (use-package go-eldoc)
    (add-hook 'go-mode-hook 'go-eldoc-require))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

;; csv
(load-lazy '(csv-mode) "csv-mode"
  (use-package csv-nav)
  (setq csv-separators '("," ";" "|" " ")))
(add-to-list 'auto-mode-alist 'csv-mode "\\.[Cc][Ss][Vv]\\'")

;; json
(load-lazy '(json-mode) "json-mode")
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; shell stuff
(load-lazy '(sh-mode) "sh-script"
  (setq sh-basic-offset tab-width)
  (add-hook 'sh-mode-hook 'whitespace-mode))

;; matlab
(load-lazy '(matlab-mode) "matlab-load"
  (add-hook 'matlab-mode
            (lambda ()
              (auto-complete-mode 1)
              (whitespace-mode)
              ))
  (setq matlab-shell-command-switches (quote ("-nodesktop -nosplash"))))
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))

;; crontab
(load-lazy '(crontab-mode) "crontab-mode")
(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))

;; mark stuff like FIXME
(load-lazy '(fic-mode) "fic-mode")
(add-hook 'prog-mode-hook     'fic-mode)
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook      'fic-mode)

;; emacs-lisp
(use-package bytecomp)
  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'byte-compile-current-buffer)

;; dired
(load-lazy '(dired-jump) "dired"
  ;; move files between split pans
  (setq dired-dwim-target t))
(load-after 'dired
  (use-package wdired)
  (use-package dired-details)
  (use-package dired-details+)
  (use-package dired-open)
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
  (define-key wdired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

  (defun dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (define-key dired-mode-map (kbd ".") 'dired-dotfiles-toggle)

  ;; open by extension
  (setq dired-open-extensions '(
                                ("pdf"  . "zathura")
                                ("djvu" . "zathura")
                                ("mkv"  . "rmpv")
                                ("mp4"  . "rmpv")
                                ))
  ;; sort number naturally
  (setq dired-listing-switches "--group-directories-first -v -al")
  )

;; mutt
;; mail support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook (lambda () (setq fill-column 72)))

;; ag search
(load-lazy '(ag) "ag"
  (setq ag-highlight-search t))

;; smart-compile
(load-lazy '(smart-compile) "smart-compile"
  (setq smart-compile-alist
        '( (emacs-lisp-mode  . (emacs-lisp-byte-compile))
           (html-mode        . (browse-url-of-buffer))
           (nxhtml-mode      . (browse-url-of-buffer))
           (html-helper-mode . (browse-url-of-buffer))
           (octave-mode      . (run-octave))
           (c-mode           . "gcc -c99 -pedantic -Wall -W -Wextra -Wunreachable-code %f")
           (java-mode        . "javac -Xlint:all -encoding UTF-8 %f")
           (if (f-exists? ".cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d")
               (haskell-mode . "ghc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d -Wall -fwarn-missing-import-lists %f")
             (haskell-mode   . "ghc -Wall -fwarn-missing-import-lists %f")
             )
           )
        ))
(global-set-key (kbd "C-S-c") 'smart-compile)

(load-lazy '(conf-mode) "conf-mode")

(load-lazy '(paradox-list-packages) "paradox"
  (setq paradox-github-token t))

(load-lazy '(nix-mode) "nix-mode"
  (add-hook 'nix-mode-hook 'whitespace-mode))
(add-to-list 'auto-mode-alist '("\\.nix" . nix-mode))

(load-lazy '(hledger-mode) "hledger-mode"
  (add-hook 'hledger-mode 'whitespace-mode))
(add-to-list 'auto-mode-alist '("\\.hledger\\.journal" . hledger-mode))

(load-lazy '(dactyl-mode) "dactyl-mode")
(add-to-list 'auto-mode-alist '("\\.pentadactylrc" . dactyl-mode))

;; magit
(load-lazy '(magit-status) "magit"
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-log-cutoff-length 1000)
  (setq magit-diff-auto-show '())
  (setq magit-push-always-verify nil)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "--ignore-space-change" magit-diff-section-arguments)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-section-arguments "--ignore-space-change")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "--ignore-space-change" magit-diff-section-arguments))
    (magit-refresh))

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))
(global-set-key (kbd "C-x g") 'magit-status)


(load-lazy '(conf-mode) "conf-mode")

(provide 'setup-major-modes)
