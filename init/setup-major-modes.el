;; major modes

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-process-popup-time -1
        magit-auto-revert-mode-lighter nil
        magit-push-always-verify nil
        magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all)
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

(use-package what-the-commit            ; Insert random commit messages
  :ensure t
  :bind (("C-c w" . what-the-commit-insert)
         ("C-c v w" . what-the-commit)))

(use-package gitconfig-mode             ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode             ; .gitignore mode
  :ensure t
  :defer t)

(use-package git-timemachine            ; go back and forward in history
  :ensure t
  :defer t)

(use-package magit-annex                ; git annex using magit
  :defer t)

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
  (setq org-use-fast-todo-selection t)  ; C-c C-<char>
  ;; priorities
  (setq org-default-priority 67) ;C
  ;; highlight math
  (setf org-highlight-latex-and-related '(latex entities))

  ;; Allow alphabetical lists
  (setq org-alphabetical-lists t)

  (defun my-org-metacontrolreturn ()
    "Execute `org-meta-return' followed by `org-meta-right'.
This usually makes new item indented one level deeper."
    (interactive)
    (org-meta-return)
    (org-metaright))
  (bind-key "<C-M-return>" 'my-org-metacontrolreturn) ; Decide whether useful only in org-mode

  (defun my-org-make-numbered-list (beg end)
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list
                    (save-excursion (outline-previous-heading) (forward-line) (point))
                    (save-excursion (outline-next-heading) (forward-line -1) (point)))))
    (string-rectangle beg end "- ")
    (beginning-of-line)
    (org-call-with-arg 'org-cycle-list-bullet 'previous)
    (org-call-with-arg 'org-cycle-list-bullet 'previous))

  ;; org keys
  (org-defkey org-mode-map (kbd "C-c 1") 'my-org-make-numbered-list)
  (org-defkey org-mode-map (kbd "C-c x") 'org-export-dispatch)
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
  (add-to-list 'org-structure-template-alist
               '("j"
                 "#+BEGIN_SRC julia\n?\n#+END_SRC"
                 "<src lang=\"julia\">\n\n</src>"))

  (setq org-default-notes-file "~/Dropbox/notes/scratch.org")
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/Dropbox/notes/scratch.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("r" "Research Journal" entry (file+datetree "~/Dropbox/notes/Research.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("n" "note" entry (file "~/Dropbox/notes/scratch.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "journal" entry (file+datetree "~/Documents/spoiler/journal.org")
                 "* %<%H:%M:%S> %?" :clock-keep t)
                ("m" "Meeting" entry (file "~/Dropbox/notes/scratch.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/Dropbox/notes/scratch.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  ;; (use-package org-download)
  (use-package org-pdfview
    :ensure t
    :defer t)
  (add-to-list 'org-file-apps '("\\.pdf\\'"                   . org-pdfview-open))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open))
  :bind (("C-. c" . org-capture))
  )


;; load raw text in a basic mode (for performance reasons)
(add-to-list 'auto-mode-alist '("\\.log$" . fundamental-mode))

;; don't let them steal you keys
(defun unbreak-stupid-map (stupid-map)
  (define-key stupid-map (kbd "C-c") nil))

;; Flycheck for code linting
(use-package flycheck
  :defer t
  :ensure t
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :init
  (global-flycheck-mode)
  :config
  (unbreak-stupid-map flycheck-mode-map)
  (use-package helm-flycheck
    :ensure t
    :after (helm))
  )


(use-package c-eldoc
  :commands (c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-buffer-regenerate-time 15))

(use-package cc-mode
  :bind ("M-RET" . c-indent-new-comment-line)
  :config
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook   'c-turn-on-eldoc-mode)
  ;; C coding style
  (setq c-default-style "linux"
        c-basic-offset tab-width
        c-block-comment-prefix "* "))

;; show what function we're in
(use-package which-func
  :ensure t
  :init
  (which-function-mode 1))

;; load ESS for R & julia
(use-package ess-site
  :mode (("\\.jl$" . ess-julia-mode)
         ("\\.R$"  . R-mode))
  :config
  (add-hook 'ess-mode-hook 'company-mode))

;; auctex
(use-package auctex
  :ensure t
  :mode ("\\.tex$" . LaTeX-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Compilation command
  (add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdf")))
  (use-package latex-extra
    :ensure t
    :init
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode)))

(use-package preview
  :commands LaTeX-preview-setup
  :init
  (setq-default preview-scale 1.4
                preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

(use-package bibtex
  :mode ("\\.bib$" . bibtex-mode)
  :init
  (setq bibtex-align-at-equal-sign t)
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))

(use-package pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :init
  (add-hook 'pdf-view-mode-hook (lambda () (nyan-mode -1)))
  :config
  (pdf-tools-install)
  (bind-keys :map pdf-view-mode-map
             ("j" . pdf-view-next-line-or-next-page)
             ("k" . pdf-view-previous-line-or-previous-page)
             ("J" . forward-page)
             ("K" . backward-page))
  (bind-keys :prefix-map pdf-annot-minor-mode-map
             :prefix "M-a"
             ("m" . pdf-annot-add-markup-annotation)
             ("s" . pdf-annot-add-squiggly-markup-annotation)
             ("u" . pdf-annot-add-underline-markup-annotation)
             ("h" . pdf-annot-add-highlight-markup-annotation))
  (add-hook 'pdf-tools-enabled-hook (lambda ()
                                      (pdf-tools-disable-minor-modes
                                       '(pdf-misc-size-indication-minor-mode))))
  (defun my-pdf-multi-extract (sources)
    "Helper function to print highlighted text from a list of pdf's, with one org header per pdf,
and links back to page of highlight."
    (let ((output ""))
      (dolist (thispdf sources)
        (setq output (concat output (pdf-annot-markups-as-org-text thispdf nil level ))))
      (princ output))
    )
  ;; this is stolen from https://github.com/pinguim06/pdf-tools/commit/22629c746878f4e554d4e530306f3433d594a654
  (defun pdf-annot-edges-to-region (edges)
    "Attempt to get 4-entry region \(LEFT TOP RIGHT BOTTOM\) from several edges.
We need this to import annotations and to get marked-up text, because annotations
are referenced by its edges, but functions for these tasks need region."
    (let ((left0 (nth 0 (car edges)))
          (top0 (nth 1 (car edges)))
          (bottom0 (nth 3 (car edges)))
          (top1 (nth 1 (car (last edges))))
          (right1 (nth 2 (car (last edges))))
          (bottom1 (nth 3 (car (last edges))))
          (n (safe-length edges)))
      ;; we try to guess the line height to move
      ;; the region away from the boundary and
      ;; avoid double lines
      (list left0
            (+ top0 (/ (- bottom0 top0) 2))
            right1
            (- bottom1 (/ (- bottom1 top1) 2 )))))
  (defun pdf-annot-markups-as-org-text (pdfpath &optional title level)
    "Acquire highligh annotations as text, and return as org-heading"
    (interactive "fPath to PDF: ")
    (let* ((outputstring "") ;; the text to be returned
           (title (or title (replace-regexp-in-string "-" " " (file-name-base pdfpath ))))
           (level (or level (1+ (org-current-level)))) ;; I guess if we're not in an org-buffer this will fail
           (levelstring (make-string level ?*)) ;; set headline to proper level
           (annots (sort (pdf-info-getannots nil pdfpath)  ;; get and sort all annots
                         'pdf-annot-compare-annotations))
           )
      ;; create the header
      (setq outputstring (concat levelstring " Quotes From " title "\n\n")) ;; create heading

      ;; extract text
      (mapc
       (lambda (annot) ;; traverse all annotations
         (if (eq 'highlight (assoc-default 'type annot))
             (let* ((page (assoc-default 'page annot))
                    ;; use pdf-annot-edges-to-region to get correct boundaries of highlight
                    (real-edges (pdf-annot-edges-to-region
                                 (pdf-annot-get annot 'markup-edges)))
                    (text (or (assoc-default 'subject annot) (assoc-default 'content annot)
                              (replace-regexp-in-string "\n" " " (pdf-info-gettext page real-edges nil pdfpath)
                                                        ) ))

                    (height (nth 1 real-edges)) ;; distance down the page
                    ;; use pdfview link directly to page number
                    (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                      "++" (number-to-string height) "][" title  "]]" ))
                    )
               (setq outputstring (concat outputstring text " ("
                                          linktext ", " (number-to-string page) ")\n\n"))
               )))
       annots)
      outputstring ;; return the header
      ))
  )

;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.md$"        . markdown-mode)
         ("\\.mkd$"       . markdown-mode)
         ("\\.pdc$"       . markdown-mode)
         ("\\.markdown$"  . markdown-mode)
         ("\\bREADME$$"   . markdown-mode))
  :config
  (setq markdown-command "pandoc --smart -f markdown -t html")
  (setq markdown-css-paths `(,(expand-file-name "markdown.css" "~/.pandoc/css/markdown.css")))
  (setq markdown-enable-math t)
  ;; add pandoc hook
  (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  (use-package pandoc-mode
    :ensure t
    :defer t
    :config
    (add-hook 'pandoc-mode-hook 'pandoc-mode))
  )

;; yaml
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode)))

;; muflax-notes
(use-package notes-mode
  :mode "\\.notes$")


;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; python
(use-package python
  :mode ("\\.py$" . python-mode)
  :init
  (defalias 'python2-mode 'python-mode)
  (defalias 'python3-mode 'python-mode)
  (defun annotate-pdb ()
    "Highlight break point lines."
    (interactive)
    (highlight-lines-matching-regexp "import i?pu?db")
    (highlight-lines-matching-regexp "i?pu?db.set_trace()"))
  (setq tab-width 2
        python-indent-offset 2
        ;; auto-indent on colon doesn't work well with if statement
        electric-indent-chars (delq ?: electric-indent-chars))
  (add-hook 'python-mode-hook #'hi-lock-mode)
  (add-hook 'python-mode-hook 'annotate-pdb)
  (defun python-setup-shell ()
    (if (executable-find "ipython")
        (progn
          (setq python-shell-interpreter "ipython")
          (when (version< emacs-version "24.4")
            ;; these settings are unnecessary and even counter-productive on emacs 24.4 and newer
            (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                  python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
                  python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
                  python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))
      (setq python-shell-interpreter "python")))
  :config
  (unbreak-stupid-map python-mode-map)
  (setq compile-command nil)
  (defun python-execute-file (arg)
    "Execute a python script in a shell."
    (interactive "P")
    ;; set compile command to buffer-file-name
    ;; universal argument put compile buffer in comint mode
    (setq universal-argument t)
    (if arg
        (call-interactively 'compile)
      (setq compile-command (format "python %s" (file-name-nondirectory
                                                 buffer-file-name)))
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode))))
  (add-hook 'python-mode-hook 'python-setup-shell)
  (add-hook 'python-mode-hook 'python-indent-guess-indent-offset)
  (use-package jedi-core
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:tooltip-method nil)
    (unbreak-stupid-map jedi-mode-map)
    (bind-keys :map jedi-mode-map
               ("M-." .  jedi:goto-definition)
               ("M-," .  jedi:goto-definition-pop-marker)
               ("M-?" .  jedi:show-doc)
               ;;("M-/" .  helm-jedi-related-names)
               ))
  (use-package py-isort
    :ensure t)
  (bind-keys :map python-mode-map
             ("C-S-c"   . python-execute-file)
             ("C-c C-f" . python-shell-send-defun)
             ("C-c C-r" . python-shell-send-region)
             ("C->"     . python-indent-shift-right)
             ("C-<"     . python-indent-shift-left))
  )


(use-package cython-mode
  :ensure t
  :defer t
  :config
  (add-hook 'cython-mode-hook (lambda () (which-func-mode -1)))
  )

(use-package ein
  :ensure t
  :defer t
  :init
  ;; (unbind-key "C-<up>" ein:notebook-mode-map)
  ;; (unbind-key "C-<down>" ein:notebook-mode-map)
  :config
  (add-hook 'ein:connect-mode-hook #'ein:jedi-setup)
  ;; (bind-keys :map ein:notebook-mode-map
  ;;            ("M-<up>" . ein:worksheet-goto-prev-input)
  ;;            ("M-<down>" . ein:worksheet-goto-next-input))
  )

;; haskell mode
(use-package haskell-mode
  :ensure t
  :mode ("\\.hs$" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-log t)
  (define-key haskell-mode-map (kbd "C-c ?") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-?") 'haskell-process-do-info)
  (use-package haskell-doc
    :diminish haskell-doc-mode)
  (use-package inf-haskell))

;; ruby ;;
;; enhanced ruby mode
(use-package enh-ruby-mode
  :ensure t
  :commands (ruby-mode enh-ruby-mode)
  :init
  ;; replace normal ruby mode
  (defalias 'ruby-mode 'enh-ruby-mode)
  :mode (("\\.rake$"    . enh-ruby-mode)
         ("Rakefile$"  . enh-ruby-mode)
         ("Gemfile$"   . enh-ruby-mode)
         ("Capfile$"   . enh-ruby-mode)
         ("\\.builder$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode))
  :interpreter ("ruby" . enh-ruby-mode)
  :config
  (setq enh-ruby-program "~/.rbenv/shims/ruby")
  ;; we use flycheck to cover errors
  (setq enh-ruby-check-syntax nil)
  ;; better indenting
  (setq ruby-indent-level tab-width)
  (setq enh-ruby-bounce-deep-indent t)
  (setq enh-ruby-deep-indent-paren nil)
  (add-hook 'enh-ruby-mode-hook 'leerzeichen-mode))


(use-package ruby-block ; show what block an end belongs to
  :ensure t
  :after (enh-ruby-mode)
  :config
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

;; lua
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :config
  (setq lua-indent-level 2))

;; (s)css
(use-package scss-mode
  :ensure t
  :mode ("\\.scss$" . scss-mode)
  :config
  (setq scss-compile-at-save nil))

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
         ("\\.php$"  . web-mode)
         ("\\.jsp$"  . web-mode)
         ("\\.jsx$"  . web-mode)
         ("\\.js$"   . web-mode)
         ("\\.xml$"  . web-mode)
         ("\\.aspx$" . web-mode))

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 2))

(load-after 'smart-compile
  (push '(web-mode . (browse-url-of-buffer)) smart-compile-alist))

;; eldoc, ie function signatures in the minibuffer
(use-package eldoc
  :diminish (eldoc-mode)
  :commands (turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil)
  (load-after 'lisp-mode
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
  )


;; go
(use-package go-mode
  :ensure t
  :mode ("\\.go$" . go-mode)
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (define-key go-mode-map (kbd "M-t") 'godef-jump)
  (define-key go-mode-map (kbd "M-T") 'godef-jump-other-window)
  (use-package go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-require))

;; csv
(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (use-package csv-nav)
  (setq csv-separators '("," ";" "|" " "))
  )

;; json
(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode))

;; shell stuff
(use-package sh-script
  :commands (sh-mode)
  :config
  (setq sh-basic-offset tab-width))

;; matlab
(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :init
  (use-package matlab-load)
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-matlab))
  (setq matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
  (eval-after-load 'flycheck
    '(require 'flycheck-matlab-mlint)))


;; crontab
(use-package crontab-mode
  :ensure t
  :mode ("\\.?cron\\(tab\\)?\\'" . crontab-mode))

(use-package pkgbuild-mode
  :ensure t
  :mode "/PKGBUILD\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "/Dockerfile\\'")

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package systemd
  :ensure t
  :mode ("\\.automount\\'\\|\\.busname\\'\\|\\.mount\\'\\|\\.service\\'\\|\\.slice\\'\\|\\.socket\\'\\|\\.target\\'\\|\\.timer\\'\\|\\.link\\'\\|\\.netdev\\'\\|\\.network\\'\\|\\.override\\.conf.*\\'" . systemd-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

;; mark stuff like FIXME
(use-package fic-mode
  :diminish fic-mode
  :commands (fic-mode))
(add-hook 'prog-mode-hook     'fic-mode)
(add-hook 'enh-ruby-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook      'fic-mode)


;; mutt
;; mail support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'mail-mode-hook (lambda () (setq fill-column 72)))


(use-package conf-mode
  :commands (conf-mode))

(use-package paradox
  :commands (paradox-list-packages)
  :config
  (setq paradox-github-token t))

(use-package nix-mode
  :mode ("\\.nix" . nix-mode)
  :commands (nix-mode)
  :config
  (add-hook 'nix-mode-hook 'leerzeichen-mode)
  )

(use-package hledger-mode
  :mode ("\\.hledger\\.journal" . hledger-mode)
  :config
  (add-hook 'hledger-mode 'leerzeichen-mode)
  )

(use-package dactyl-mode
  :mode ("\\.pentadactylrc$" . dactyl-mode)
  :commands (dactyl-mode))

(use-package semantic
  :defer t
  :init
  (setq semanticdb-default-save-directory (emacs-d "cache/semanticdb/"))
  (unless (file-exists-p semanticdb-default-save-directory)
    (make-directory semanticdb-default-save-directory))
  (add-hook 'prog-mode-hook (lambda ()
                              (add-to-list 'semantic-default-submodes
                                           'global-semantic-idle-summary-mode)
                              (semantic-mode 1))))

;; Automatic Math preview toggle in org-mode
;; Source: http://goo.gl/WLYzxp
(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")
;; FIXME Pretty janky right now
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
           (let ((ov (loop for ov in (org--list-latex-overlays)
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
           (let ((ov (loop for ov in (org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           return ov)))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))
(add-hook 'post-command-hook 'org-latex-fragment-toggle)

;; Should load this after all other languages I guess :(
(use-package org-plus-contrib
  :ensure t
  :config
  ;; code block
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (R          . t)
     (matlab     . t)
     (julia      . t)
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
  (setq org-confirm-babel-evaluate nil))

(use-package ag                         ; ag search
  :ensure t
  :commands (ag helm-do-grep-ag)
  :config
  (setq ag-highlight-search t)
  )
(use-package wgrep-ag                   ; Wgrep for ag
  :ensure t
  :defer t
  :config
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))


;; smart-compile
(use-package smart-compile
  :bind (("C-S-c" . smart-compile))
  :config
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
             (haskell-mode   . "ghc -Wall -fwarn-missing-import-lists %f"))
           )
        ))

;; emacs-lisp
(use-package bytecomp
  :config
  (defun byte-compile-current-buffer ()
    "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'byte-compile-current-buffer))

;; dired
(use-package dired
  :commands (dired-jump)
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t)
  (use-package wdired
    :ensure t)
  (use-package dired-details
    :ensure t)
  (use-package dired-details+
    :ensure t)
  (use-package dired-open
    :ensure t)
  (use-package dired-narrow             ; narrow dired to match filter
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))

  ;; reload dired after making changes
  (--each '(dired-do-rename
            dired-do-copy
            dired-create-directory
            wdired-abort-changes)
    (eval `(defadvice ,it (after revert-buffer activate)
             (revert-buffer))))
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
                                ("djvu" . "zathura")
                                ("mkv"  . "rmpv")
                                ("mp4"  . "rmpv")
                                ("mp3"  . "rmpv -a")
                                ))
  ;; sort number naturally
  (setq dired-listing-switches "--group-directories-first -v -al")
  ;; also revert dired
  (add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
  )

;; git-annex
(use-package git-annex)

(provide 'setup-major-modes)
