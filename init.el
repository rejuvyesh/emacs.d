(setq user-full-name "Jayesh Kumar Gupta"
      user-mail-address "jayeshkg@iitk.ac.in")

;; package-repositories
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; https://github.com/dimitri/el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")  ; This is configured at the top.
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
    (goto-char (point-max))
    (eval-print-last-sexp))))
(el-get 'sync)

;; Make sure a package is installed
(defun package-require (package)
    "Install a PACKAGE unless it is already installed 
or a feature with the same name is already active.
Usage: (package-require 'package)"
                                        ; try to activate the package with at least version 0.
    (package-activate package '(0))
                                        ; try to just require the package. Maybe the user has it in his local config
    (condition-case nil
        (require package)
                                        ; if we cannot require it, it does not exist, yet. So install it.
      (error (package-install package))))
(package-initialize)
(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
(package-refresh-contents))
; (package-refresh-contents)
;; define custom lisp directory and load all subdirectories too
(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

; try to keep windows within a max margin
(require 'automargin)
(setq automargin-target-width 120)
(automargin-mode)
;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c k") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-k") 'mc/mark-all-like-this)
(require 'phi-search)
(require 'phi-search-mc)
(define-key phi-search-default-map (kbd "C-c >") 'phi-search-mc/mark-next)
(define-key phi-search-default-map (kbd "C-c <") 'phi-search-mc/mark-previous)
(define-key phi-search-default-map (kbd "C-c C-k") 'phi-search-mc/mark-all)

;; undo highlighting
(require 'volatile-highlights)
(volatile-highlights-mode t)

(ido-mode 1)
  (setq ido-default-buffer-method 'selected-window)
  (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
  (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
  (require 'ido-ubiquitous)
  (setq ido-enable-flex-matching t) ; fuzzy matching
  (setq ido-everywhere t)
  (setq ido-use-virtual-buffers t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (setq ido-case-fold t) ; case insensitive
  (defun ido-sort-mtime ()
    (setq ido-temp-list
          (sort ido-temp-list 
                (lambda (a b)
                  (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                        (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                    (if (= (nth 0 ta) (nth 0 tb))
                        (> (nth 1 ta) (nth 1 tb))
                      (> (nth 0 ta) (nth 0 tb)))))))
    (ido-to-end  ;; move . files to end (again)
     (delq nil (mapcar
                (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                ido-temp-list))))

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)   

;; remove the toolbar which no-one uses :)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq org-completion-use-ido t)

;; Flycheck for code linting
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; load ESS for R
(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(require 'ess-site)

;; auctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(setq-default TeX-engine 'xetex)      ; use xelatex by default
(setq TeX-view-program-selection '((output-pdf "zathura")))
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources))
    )

;; ;; python ;;
;; ;; ipython as shell
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;  "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;  "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)

;; python-mode
;; (require 'python-mode)
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; jedi completion
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'linum-mode)

;; Sometimes you have to
(require 'php-mode)

;; javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; lua
(require 'lua-mode)
(setq lua-indent-level 2)

;; (s)css
(setq scss-compile-at-save nil)
(setq css-indent-level 2)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "<C-prior>") 'er/expand-region)
(global-set-key (kbd "<C-next>") 'er/contract-region)

;; C coding style
(setq c-default-style "linux"
      c-basic-offset 4)
(add-hook 'c-mode-hook
          (lambda ()
          (add-to-list 'ac-sources 'ac-source-c-headers)
          (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
;; scrolling
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-progressive-speed nil)
;; smooth scrolling with margin
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
;; necessary or scrolling is really slow
(setq-default bidi-display-reordering nil)
(setq auto-window-vscroll nil)

;; I like M-g for goto-line
(global-set-key "\M-g" 'goto-line)

;;M-down and M-up do nothing! :(  Let's make them do something, like M-left  and M-right do.

(global-set-key [M-down] '(lambda () (interactive) (progn (forward-line 10) (recenter) ) ))
(global-set-key [M-up]   '(lambda () (interactive) (progn (forward-line -10) (recenter) ) ))

;; snippets
(setq yas-snippet-dirs "~/.emacs.d/snippets")
                       
(require 'yasnippet)
(define-key yas-minor-mode-map [backtab] 'yas-next-field)
(define-key yas-minor-mode-map [(shift tab)] 'yas-next-field)
(define-key yas-minor-mode-map [(control tab)] 'yas-prev-field)
(yas-global-mode 1)

; auto-yasnippet
(require 'auto-yasnippet)
(global-set-key (kbd "C-c ~") 'aya-create)
(global-set-key (kbd "C-c C-~") 'aya-expand)


;; text completion

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; auto completion
(require 'auto-complete-config)
(ac-config-default)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
;; extra modes auto-complete must support
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                    html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                    lisp-mode textile-mode markdown-mode tuareg-mode
                                    js2-mode css-mode less-css-mode))
  (add-to-list 'ac-modes mode))
(setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
                                        ;(setq ac-use-menu-map t)
;; disabling Yasnippet completion
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))
(defun epy-get-all-snips ()
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas--tables)
    (apply 'append candidates))
  )
(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))

;; recent files
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-save-file "~/.emacs.d/cache/recentf")
(setq recentf-exclude (append recentf-exclude
                              '("\.emacs\.d/cache"
                                "\.emacs\.d/elpa")))
(recentf-mode 1)

;; file completion
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key "\C-x\C-r" 'recentf-ido-find-file)
;; stop tramp file competions
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "0a47a318b366c8d5bf2a4738ff4cea9988c60f4b3b7f7a31cff565a7889406a5" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "2282f550e7baf0ff8eb1432983676d20fed81a2fdd8c50d70e62cb58580633f4" "843a82ff3b91bec5430f9acdd11de03fc0f7874b15c1b6fbb965116b4c7bf830" "5e2ade7f65d9162ca2ba806908049fb37d602d59d90dc3a08463e1a042f177ae" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" default)))
 '(ecb-fix-window-size (quote auto))
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes (quote (("left14" (ecb-speedbar-buffer-name 33 . 45) (ecb-history-buffer-name 33 . 23)) ("left15" (ecb-speedbar-buffer-name 33 . 34) (ecb-methods-buffer-name 33 . 34)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-process-non-semantic-files t)
 '(ecb-tip-of-the-day nil)
 '(ecb-toggle-layout-sequence (quote ("left14" "left15")))
 '(ecb-use-speedbar-instead-native-tree-buffer (quote dir))
 '(ecb-window-width 33)
 '(evernote-developer-token "S=s162:U=117b1be:E=1496c388c33:C=14214876037:P=1cd:A=en-devtoken:V=2:H=b31dbef3ff74c8b74ff117549bd396af")
 '(ido-enable-tramp-completion nil)
 '(matlab-shell-command-switches (quote ("-nodesktop -nosplash")))
 '(linum-format " %7i ")
 '(safe-local-variable-values nil))

;; disable tramp
(setq tramp-mode nil)

;; safety
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosave-dir/"))
(setq auto-save-list-file-prefix "~/.emacs-saves/cache/auto-save-list/.saves-")
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)

;; save location inside buffer
(require 'saveplace)
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)

;; optical stuff
(blink-cursor-mode -1)
(setq inhibit-splash-screen t)
;; power line
(require 'powerline)
(setq powerline-arrow-shape 'arrow14)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#98c1d1" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#98b3d1" :box nil)))))

;; shows current selected region
(setq-default transient-mark-mode t)
(set-scroll-bar-mode 'right)
(setq frame-title-format "%b - emacs")
(set-fringe-mode '(0 . 1))

;; text stuff
(setq default-major-mode 'org-mode)
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

;; deltete selected
(delete-selection-mode t)

;; evernote-mode
(require 'evernote-mode)
(setq evernote-username "jayeshkg") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;; don't hard-wrap text, but use nice virtual wrapping
(setq-default fill-column 80)
(global-visual-line-mode 1)
(require 'adaptive-wrap-prefix)
(global-adaptive-wrap-prefix-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; parentheses are connected and their content highlighted
(show-paren-mode 1)
(setq blink-matching-paren-distance nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses () (highlight-parentheses-mode 1))
(add-hook 'emacs-lisp-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'lisp-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'java-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'python-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'c-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'haskell-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'text-mode-hook 'turn-on-highlight-parentheses)

;; key bindings
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key (kbd "C-c SPC") 'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-ci" 'indent-region)

(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\M-f" 'forward-sentence)
(global-set-key "\M-b" 'backward-sentence)

;; if no region is active, act on current line
(require 'whole-line-or-region)
(setq whole-line-or-region-extensions-alist
      '((comment-dwim whole-line-or-region-comment-dwim-2 nil)
        (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
        (kill-region whole-line-or-region-kill-region nil)
        (kill-ring-save whole-line-or-region-kill-ring-save nil)
        (yank whole-line-or-region-yank nil)
        ))
(whole-line-or-region-mode 1)

;; better rectangle functionality
(require 'phi-rectangle)
(phi-rectangle-mode)

;; edit symbol in multiple places simultaneously
(require 'iedit)
(global-set-key "\C-ce" 'iedit-mode)

;; save minibuffer history
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/cache/history")
(setq savehist-additional-variables '(search-ring
                                       regexp-search-ring
                                       kill-ring
                                       compile-command))

;; reload file when it changed (and the buffer has no changes)
(global-auto-revert-mode 1)

;; indentation
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
;; automatically turn on indenting
(electric-indent-mode 1)
;; also when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

; mark stuff like FIXME
(require 'fic-mode)
(global-fic-mode 1)

;; csv
(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(autoload 'csv-nav-mode "csv-nav-mode" "Major mode for navigating comma-separated value files." t)

(setq csv-separators '("," ";" "|" " "))


;; markdown
(require 'markdown-mode)
(setq markdown-command "pandoc --smart -f markdown -t html")
(add-to-list 'auto-mode-alist '("\\.pdc$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\bREADME$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\/.page$" . markdown-mode))
;; add pandoc hook
(add-hook 'markdown-mode-hook 'turn-on-pandoc)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(defun no-electric-indent-yaml ()
  (electric-indent-mode -1)
  (define-key yaml-mode-map [(return)] 'newline-and-indent))
(add-hook 'yaml-mode-hook 'no-electric-indent-yaml)

;; json
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
;; octave mode
;; (autoload 'octave-mode "octave-mod" nil t)
;; (setq auto-mode-alist
;;       (cons '("\\.m$" . octave-mode) auto-mode-alist))

(load-library "matlab-load")

(add-hook 'matlab-mode
          (lambda ()
            (auto-complete-mode 1)
            ))
(setq auto-mode-alist
      (cons
       '("\\.m$" . matlab-mode)
            auto-mode-alist))

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(when (eval-when-compile (>= emacs-major-version 24))
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; org-mode
(setq org-special-ctrl-a/e t)
(setq load-path (cons "~/.emacs.d/site-lisp/org-mode/lisp" load-path))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; loaded so that we can diminish it later
(require 'org-indent)
;; proper indentation / folding
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-indent-indentation-per-level 2)
(setq org-startup-folded 'content)
(setq org-blank-before-new-entry '(
  (heading . nil)
  (plain-list-item . auto)))
;; tag column
(setq org-tags-column -70)
;; dependencies
(setq org-enforce-todo-dependencies t)
;; make clock history persistent
(setq org-clock-persist 'history)
(setq org-clock-persist-file "~/.emacs.d/cache/org-clock-save.el")
(org-clock-persistence-insinuate)
;; spoiler files
(defadvice org-todo-list (before org-todo-list-reload activate compile)
  "Scan for org files whenever todo list is loaded."
  ; 'find' is faster and has better control than lisp
  (setq org-agenda-files (mapcar 'abbreviate-file-name (split-string
    (shell-command-to-string "find ~/Documents/spoiler -type f -name \"*.org\" | sort")
      "\n"))))
;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)")))
;; priorities
(setq org-default-priority 67) ;C
;; code block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (R . t)
   (matlab . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (haskell . t)))
(add-to-list 'org-src-lang-modes '("c" . c))
(add-to-list 'org-src-lang-modes '("r" . ess-mode))
(add-to-list 'org-src-lang-modes '("h" . haskell))
(add-to-list 'org-src-lang-modes '("s" . sh))
(add-to-list 'org-src-lang-modes '("p" . python))
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
;; keybindings
(define-key global-map "\C-ct" 'org-todo-list)
(org-defkey org-mode-map "\C-c\C-t" (lambda () (interactive) (org-todo "TODO")))
(org-defkey org-mode-map "\C-c\C-w" (lambda () (interactive) (org-todo "WAITING")))
(org-defkey org-mode-map "\C-c\C-d" (lambda () (interactive) (org-todo "DONE")))
;; shortcut for C-u C-c C-l
(defun org-insert-file-link () (interactive) (org-insert-link '(4)))
(define-key global-map "\C-cf" 'org-insert-file-link)
(define-key global-map "\C-cl" 'org-store-link)
;; go to last position before C-c C-o
(define-key global-map "\C-co" 'org-mark-ring-goto)
;; some templates
(setcdr (assoc "c" org-structure-template-alist)
        '("#+BEGIN_COMMENT\n?\n#+END_COMMENT"))
(add-to-list 'org-structure-template-alist
             '("r"
               "#+BEGIN_SRC ruby\n?\n#+END_SRC"
               "<src lang=\"ruby\">\n\n</src>"))

(require 'diminish)
(diminish 'highlight-parentheses-mode)
(diminish 'fic-mode)
(diminish 'auto-complete-mode "AC")
(diminish 'undo-tree-mode)
(diminish 'visual-line-mode)
(diminish 'volatile-highlights-mode)
(diminish 'whole-line-or-region-mode)
(diminish 'yas-minor-mode)

;; scratchpad buffers
(require 'scratch)
;; don't want to remember which key I used
(global-set-key (kbd "C-c b") 'scratch)
;; don't start in lisp
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; oh pretty!
(require 'pretty-lambdada)
(global-pretty-lambda-mode)

;; show #colors in matching color
(require 'rainbow-mode)

;; support for bookmarks
(require 'breadcrumb)
(global-set-key (kbd "C-c m") 'bc-set)
(global-set-key (kbd "M-SPC") 'bc-previous)
(global-set-key (kbd "M-S-SPC") 'bc-next)
(setq bc-bookmark-limit 1000)
(setq bc-bookmark-file (expand-file-name "~/.emacs.d/cache/breadcrumb"))
;; normal bookmarks
(setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; unset unwanted keys
(when (eq window-system 'x)
  (if (eq (key-binding "\C-x\C-z") 'suspend-frame)
      (global-unset-key "\C-x\C-z"))
  (if (eq (key-binding "\C-z") 'suspend-frame)
      (global-unset-key "\C-z")))
(if (eq (key-binding [(insert)]) 'overwrite-mode)
    (global-unset-key [(insert)]))
(if (eq (key-binding [(insertchar)]) 'overwrite-mode)
    (global-unset-key [(insertchar)]))

;; semantic (code parser)
(require 'semantic)
(setq semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")
(semantic-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-idle-completions-mode 1)

;; ecb (code browser)
(require 'ecb-autoloads)
;; fix for emacs 24
(unless (boundp 'stack-trace-on-error)
  (defvar stack-trace-on-error nil))

   
;; keys
(global-set-key "\C-c\C-t" 'ecb-toggle-layout)
(global-set-key "\C-c;" 'ecb-minor-mode)


;; I never use set-fill-column and I hate hitting it by accident.
(global-set-key "\C-x\ f" 'find-file)

;;Make completion buffers in a shell disappear after 10 seconds.
;;<http://snarfed.org/space/why+I+don't+run+shells+inside+Emacs>
(add-hook 'completion-setup-hook
          (lambda () (run-at-time 10 nil
                                  (lambda () (delete-windows-on "*Completions*")))))

;; highlight current line
(defface hl-line '((t (:background "aquagreen")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)


;; add current date
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d %B, %Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d %B, %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; spell-checking
(require 'wcheck-mode)
(setq ispell-really-hunspell t)
(setq wcheck-timer-idle .2)
(define-key global-map "\C-cs" 'wcheck-actions)
(setq-default
 wcheck-language "English"
 wcheck-language-data '(("English"
                         (program . "/usr/bin/enchant")
                         (args . ("-l" "-d" "en_US"))
                         (action-program . "/usr/bin/enchant")
                         (action-args "-a" "-d" "en_US")
                         (action-parser . enchant-suggestions-menu))
                        ))

 ;; add to dictionary functionality
 (defun enchant-suggestions-menu (marked-text)
   (cons (cons "[Add]" 'enchant-add-to-dictionary)
         (wcheck-parser-ispell-suggestions)))

 (defvar enchant-dictionaries-dir "~/.config/enchant")

 (defun enchant-add-to-dictionary (marked-text)
   (let* ((word (aref marked-text 0))
          (language (aref marked-text 4))
          (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
                                                 language 'action-args)))))
                  (when (stringp code)
                    (concat (file-name-as-directory enchant-dictionaries-dir)
                            code ".dic")))))

     (when (and file (file-writable-p file))
       (with-temp-buffer
         (insert word) (newline)
         (append-to-file (point-min) (point-max) file)
         (message "Added word \"%s\" to the %s dictionary"
                  word language)))))

 ;; enable spell-check in certain modes
 (defun turn-on-spell-check ()
   (wcheck-mode 1))
 (add-hook 'text-mode-hook 'turn-on-spell-check)
 (add-hook 'markdown-mode-hook 'turn-on-spell-check)
 (add-hook 'org-mode-hook 'turn-on-spell-check)

;; use automatic file headers
(load "~/.emacs.d/emacs-auto-insert.el")

;; mutt
;; mail support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))
(add-hook 'mail-mode-hook (lambda () (setq fill-column 72)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove background color from terminal emacs,
;; so that it can remain transparent
;; http://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;; color

;; using modified molokai theme
(load-theme 'molokai t)
(icomplete-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some saner clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking out helm
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-t") 'helm-cmd-t)
(global-set-key [remap switch-to-buffer] 'helm-C-x-b)
(global-set-key (kbd "C-x c g") 'helm-do-grep)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(setq helm-ff-lynx-style-map nil
      helm-input-idle-delay 0.1
      helm-idle-delay 0.1
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go to last change
;; http://www.emacswiki.org/emacs/GotoChg
(require 'goto-chg)
(global-set-key [f8] 'goto-last-change)

;; rainbow-delimiters.el
;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
(add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; for programming related modes

;; yascroll
(global-yascroll-bar-mode 1)

;; better search/replace
(require 'visual-regexp)
(require 'visual-regexp-steroids)
(global-set-key "\C-cr" 'vr/query-replace)

;; copy end of line, like C-k
(defun copy-line ()
  (interactive)
  (set 'this-command 'copy-to-kill)
  (save-excursion
    (set-mark (point))
    (if (= (point) (line-end-position))
        (forward-line)
      (goto-char (line-end-position)))
    (if (eq last-command 'copy-to-kill)
        (append-next-kill))
    (kill-ring-save (mark) (point))))
(global-set-key "\M-k" 'copy-line)

;; move to beginning of text on line
(defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line. If point was
already at that position, move point to beginning of line.

If visual-line-mode is on, then also jump to beginning of real line."
    (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
    (let ((oldpos (point))
          (vispos (point)))

      (beginning-of-visual-line)
      (setq vispos (point))
      (beginning-of-line-text)

      (if (and (> vispos (point))
               (not (= oldpos vispos)))
          (goto-char vispos)
        (when (= oldpos (point))
          (beginning-of-line)))))
(global-set-key "\C-a" 'smart-beginning-of-line)

(defun smart-end-of-line ()
  "Move point to end of visual line or, if already there, to end of logical line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))

    (end-of-visual-line)
    (when (= oldpos (point))
      (end-of-line))))
(global-set-key "\C-e" 'smart-end-of-line)

;; semi-port of surround.vim
(require 'surround)
(global-surround-mode 1)
(global-set-key "\C-cd" 'surround-delete)
(global-set-key "\C-cD" 'surround-delete-within)
(global-set-key "\C-c\M-d" 'surround-change)

;; move lines like in org-mode
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; fonts
(defvar small-font "6x10")
(defvar normal-font "6x13")
(defvar big-font "-gnu-unifont-*")
(defvar font-list (list
                   small-font
                   normal-font
                   big-font))
(defvar current-font normal-font)

(defun set-window-font ()
  (set-frame-font current-font))
(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

(defun cycle-fonts ()
  "cycles through font list"
  (interactive)

  (let (currentState)
    ;; states starts from 1.
    (setq currentState (if (get this-command 'state) (get this-command 'state) 1))
    (setq current-font (nth (1- currentState) font-list))
    (put this-command 'state (1+ (% currentState (length font-list))))
    (set-window-font)))
(global-set-key "\C-c\C-f" 'cycle-fonts)

;; shortcut for the fonts
(defun use-big-font ()
  "use big font"
  (interactive)
  (setq current-font big-font)
  (set-window-font))
(defun use-normal-font ()
  "use normal font"
  (interactive)
  (setq current-font normal-font)
  (set-window-font))
(defun use-small-font ()
  "use small font"
  (interactive)
  (setq current-font small-font)
  (set-window-font))
(global-set-key (kbd "C-c <f1>") 'use-small-font)
(global-set-key (kbd "C-c <f2>") 'use-normal-font)
(global-set-key (kbd "C-c <f3>") 'use-big-font)

;; More emacs rocks: Join current line with the next
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

;; Find init file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Guide Key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; ace-jump (hint-style navigation)
(require 'ace-jump-mode)
(global-set-key (kbd "C-c j") 'ace-jump-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "<C-prior>") 'er/expand-region)
(global-set-key (kbd "<C-next>") 'er/contract-region)
