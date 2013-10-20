(setq user-full-name "Jayesh Kumar Gupta"
      user-mail-address "jayeshkg@iitk.ac.in")

;; package-repositories
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; define custom lisp directory and load all subdirectories too
(let ((default-directory "~/.emacs.d/site-lisp/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


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

(setq org-completion-use-ido t)

;; load ESS for R
(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
(require 'ess-site)
;; R flymake support (if Flymake is available) This will call a script
;; "rflymake" with the path given; make sure it is on emac's exec-path
;; or give a full path.
(when (require 'flymake nil)
  (defun flymake-r-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/Scripts/rflymake" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.[Rr]\\'" flymake-r-init))
  (add-to-list 'flymake-err-line-patterns
               '("parse(\"\\([^\"]*\\)\"): \\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                 1 2 3 4))
  (add-hook 'r-mode-hook 'flymake-mode)
  )
(require 'r-autoyas)
(add hook 'ess-mode-hook 'r-autoyas-ess-activate)

;; auctex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

;; python ;;
;; ipython as shell
(require 'ipython)
;; python-mode
(require 'python-mode)
(autoload 'python-mode "python-mode" "Python editing mode." t)
; On-the-fly syntax checking via flymake
(eval-after-load 'python
  '(require 'flymake-python-pyflakes))

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;; Sometimes you have to
(require 'php-mode)

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

;; text completion




;; auto completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete.el/ac-dict")
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
 '(custom-safe-themes (quote ("0a47a318b366c8d5bf2a4738ff4cea9988c60f4b3b7f7a31cff565a7889406a5" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "2282f550e7baf0ff8eb1432983676d20fed81a2fdd8c50d70e62cb58580633f4" "843a82ff3b91bec5430f9acdd11de03fc0f7874b15c1b6fbb965116b4c7bf830" "5e2ade7f65d9162ca2ba806908049fb37d602d59d90dc3a08463e1a042f177ae" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" default)))
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
 '(ido-enable-tramp-completion nil)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#2d5565" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; shows current selected region
(setq-default transient-mark-mode t)
(set-scroll-bar-mode 'right)

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

;; octave mode
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(when (eval-when-compile (>= emacs-major-version 24))
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(require 'diminish)
(diminish 'highlight-parentheses-mode)
(diminish 'fic-mode)
(diminish 'auto-complete-mode "AC")

;; don't start in lisp
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

;; oh pretty!
(require 'pretty-lambdada)
(global-pretty-lambda-mode)

;; show #colors in matching color
(require 'rainbow-mode)

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
(global-set-key "\C-cf" 'semantic-ia-show-summary)

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

;; using prebulit cyberpunk theme
(load-theme 'cyberpunk t)
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
