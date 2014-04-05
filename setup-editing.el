;; generic editing features

;; safety first
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name "~/.emacs.d/cache/autosave-dir/"))
(setq auto-save-list-file-prefix "~/.emacs-saves/cache/auto-save-list/.saves-")
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)
;; move files to trash instead
(setq delete-by-moving-to-trash t)

;; save minibuffer history
(savehist-mode 1)
(setq history-length 1000)
(setq search-ring-max 1000)
(setq regexp-search-ring-max 1000)
(setq savehist-file "~/.emacs.d/cache/history")
(setq savehist-additional-variables '(search-ring
                                      regexp-search-ring
                                      kill-ring
                                      compile-command))

;; save location inside buffer
(setup "saveplace"
  (setq save-place-file "~/.emacs.d/cache/saveplace")
  (setq-default save-place t))

;; more useful kill-ring
(setq kill-ring-max 2000)
(setup "kill-ring-search"
  (global-set-key "\M-\C-y" 'kill-ring-search))
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key "\M-Y" 'yank-pop-reverse)

;; text stuff
(setup-expecting "org-mode"
  (setq default-major-mode 'org-mode))
(prefer-coding-system 'utf-8)
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

;; don't hard-wrap text, but use nice virtual wrapping
(setup "adaptive-wrap"
  (setq-default fill-column 80)
  (global-visual-line-mode 1)
  (global-adaptive-wrap-prefix-mode 1)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)))

;; enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; general key bindings
(global-set-key (kbd "C-c c")    'comment-region)
(global-set-key (kbd "C-c u")    'uncomment-region)
(global-set-key (kbd "C-c SPC")   'comment-dwim)
(global-set-key (kbd "C-c C-SPC") 'comment-dwim)
(global-set-key (kbd "C-c n")    'next-error)
(global-set-key (kbd "C-c p")    'previous-error)
(global-set-key (kbd "C-c i")    'indent-region)
(global-set-key (kbd "M-g")      'goto-line)
(global-set-key (kbd "C-S-M-x")   'eval-buffer)

(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M-z") 'undo-tree-redo)

(global-set-key (kbd "C-f")   'forward-word)
(global-set-key (kbd "C-b")   'backward-word)
(global-set-key (kbd "M-f")   'forward-sentence)
(global-set-key (kbd "M-b")   'backward-sentence)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>")  'end-of-buffer)


;; allowed key components
(setup "free-keys"
  (setq free-keys-keys
        (concat "abcdefghijklmnopqrstuvwxyz"
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                "1234567890"
                "!@#$%^&*()-=[]{};'\\:\"|,./<>?`~_+"
                )))
;; unset unwanted default keys
(cl-loop for key in `(
                   (,(kbd "C-x C-z") suspend-frame)
                   (,(kbd "C-z") suspend-frame)
                   ([(insert)] overwrite-mode)
                   ([(insertchar)] overwrite-mode)
                   (,(kbd "C-]") abort-recursive-edit)
                   (,(kbd "C-@") set-mark-command)
                   (,(kbd "<C-down-mouse-1>") mouse-buffer-menu)
                   (,(kbd "<C-down-mouse-2>") facemenu-menu)
                   (,(kbd "<S-down-mouse-1>") mouse-appearance-menu)
                   (,(kbd "C-x C-t") transpose-lines)
                   (,(kbd "C-x C-q") read-only-mode)
                   (,(kbd "C-x C-o") delete-blank-lines)
                   (,(kbd "C-x C-n") set-goal-column)
                   (,(kbd "C-x TAB") indent-rigidly)
                   (,(kbd "C-x C-e") eval-last-sexp)
                   (,(kbd "C-x C-d") list-directory)
                   (,(kbd "C-x C-@") pop-global-mark)
                   (,(kbd "C-x SPC") gud-break)
                   (,(kbd "C-x #") server-edit)
                   (,(kbd "C-x $") set-selective-display)
                   (,(kbd "C-x '") expand-abbrev)
                   (,(kbd "C-x <") scroll-left)
                   (,(kbd "C-x =") what-cursor-position)
                   (,(kbd "C-x >") scroll-right)
                   (,(kbd "C-x [") backward-page)
                   (,(kbd "C-x ]") forward-page)
                   (,(kbd "C-x ^") enlarge-window)
                   (,(kbd "C-x `") next-error)
                   (,(kbd "C-x l") count-lines-page)
                   (,(kbd "C-x {") shrink-window-horizontally)
                   (,(kbd "C-x }") enlarge-window-horizontally)
                   (,(kbd "C-M-@") mark-sexp)
                   (,(kbd "C-M-d") down-list)
                   (,(kbd "C-M-l") reposition-window)
                   (,(kbd "C-M-n") forward-list)
                   (,(kbd "C-M-p") backward-list)
                   (,(kbd "C-M-t") transpose-sexps)
                   (,(kbd "C-M-u") backward-up-list)
                   (,(kbd "C-M-v") scroll-other-window)
                   (,(kbd "C-M-\\") indent-region)
                   (,(kbd "M-$") ispell-word)
                   (,(kbd "M-%") query-replace)
                   (,(kbd "M-'") abbrev-prefix-mark)
                   (,(kbd "M-(") insert-parentheses)
                   (,(kbd "M-)") move-past-close-and-reindent)
                   (,(kbd "M-*") pop-tag-mark)
                   (,(kbd "M-.") find-tag)
                   (,(kbd "M-,") tags-loop-continue)
                   (,(kbd "M-/") dabbrev-expand)
                   (,(kbd "M-=") count-words-region)
                   (,(kbd "M-@") mark-word)
                   (,(kbd "M-\\") delete-horizontal-space)
                   (,(kbd "M-`") tmm-menubar)
                   (,(kbd "M-a") backward-sentence)
                   (,(kbd "M-e") forward-sentence)
                   (,(kbd "M-m") back-to-indentation)
                   (,(kbd "M-o") facemenu-keymap)
                   (,(kbd "M-r") move-to-window-line-top-bottom)
                   (,(kbd "M-{") backward-paragraph)
                   (,(kbd "M-}") forward-paragraph)
                   (,(kbd "M-~") not-modified)
                   (,(kbd "C-M-S-v") scroll-other-window-down)
                   (,(kbd "C-M-%") query-replace-regexp)
                   (,(kbd "C-M-.") find-tag-regexp)
                   (,(kbd "C-M-/") dabbrev-completion)
                   )
      collect (if (eq (key-binding (first key)) (second key))
                  (global-unset-key (first key))))

;; multiple cursors
(setup "multiple-cursors"
  (global-set-key (kbd "C-c d")         'mc/edit-lines)
  (global-set-key (kbd "<C-down>")      'mc/mark-next-like-this)
  (global-set-key (kbd "<C-up>")        'mc/mark-previous-like-this)
  (global-set-key (kbd "<M-C-down>")    'mc/skip-to-next-like-this)
  (global-set-key (kbd "<M-C-up>")      'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C-d")       'mc/mark-all-dwim)
  (global-set-key (kbd "C-c >")         'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-c <")         'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-<down-mouse-1>") 'mc/add-cursor-on-click))
(setup-after "multiple-cursors-core"
  (define-key mc/keymap (kbd "<return>") nil)
  (define-key mc/keymap (kbd "C-j") 'multiple-cursors-mode))
(setup "phi-search"
  (global-set-key (kbd "C-c C-s") 'phi-search)
  (global-set-key (kbd "C-c C-r") 'phi-search-backward))
(setup-after "phi-search"
  (setup "phi-search-mc"
    (define-key phi-search-default-map (kbd "<C-down>") 'phi-search-mc/mark-next)
    (define-key phi-search-default-map (kbd "<C-up>")   'phi-search-mc/mark-previous)
    (define-key phi-search-default-map (kbd "C-c C-k")  'phi-search-mc/mark-all)))

;; edit symbol in multiple places simultaneously
(setup-lazy '(iedit-mode) "iedit"
  (global-set-key (kbd "C-c ;") 'iedit-mode)
  (global-set-key (kbd "C-c C-;") 'iedit-mode-toggle-on-function))

;; undo tree
(setup "undo-tree"
  (global-undo-tree-mode))

;; nested parentheses are highlighted when inside of them
(require 'highlight-parentheses)
(defun turn-on-highlight-parentheses () (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'enh-ruby-mode-hook 'turn-on-highlight-parentheses)
(add-hook 'text-mode-hook 'turn-on-highlight-parentheses)

;; better search/replace
(setup "visual-regexp")
(setup-after "visual-regexp"
  (setup "visual-regexp-steroids")
  (global-set-key (kbd "C-c r") 'vr/query-replace)
  (defun vr/query-replace-from-beginning ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (call-interactively 'vr/query-replace)))
  (global-set-key (kbd "C-c R") 'vr/query-replace-from-beginning))

;; search info
(setup "anzu"
  (global-anzu-mode t))

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

;; org-mode has similar behavior built-in, so use it instead
(setq org-special-ctrl-a/e t)

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

Prefixed with \\[universal-argument], expand the file name to its full path."
  (interactive "*fInsert file name: \nP")
  (cond ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert (file-relative-name filename)))))
(global-set-key "\C-c\C-i" 'insert-file-name)

(setup "ido"
  (ido-mode 1)
  
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-enable-flex-matching t) ; fuzzy matching
  (setq ido-use-virtual-buffers t)
  (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
  (setq ido-case-fold t) ; case insensitive
  (setq ido-enable-last-directory-history t)
  (setq ido-max-work-directory-list 30))
(setup-after "ido"
  (setup "ido-ubiquitous"
    (setq ido-everywhere t))
  (setup-expecting "org-mode"
    (setq org-completion-use-ido t)))

;; helm
(setup "helm-config"
  (helm-mode t)
  (global-set-key (kbd "M-t") 'helm-cmd-t)
  (global-set-key (kbd "C-x c g") 'helm-do-grep)
  (global-set-key (kbd "C-x c o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (setq helm-ff-lynx-style-map nil
        helm-input-idle-delay 0.1
        helm-idle-delay 0.1 ))

(setup-lazy '(helm-C-x-b) "helm-C-x-b"
  (global-set-key [remap switch-to-buffer] 'helm-C-x-b))


;; recent files
(setup "recentf"
  (setq recentf-max-saved-items 1000)
  (setq recentf-save-file "~/.emacs.d/cache/recentf")
  (setq recentf-exclude (append recentf-exclude
                                '("\.emacs\.d/cache"
                                  "\.emacs\.d/elpa")))
  (setq recentf-keep '(file-remote-p file-readable-p))
  (recentf-mode 1))
;; file completion
(setup-after "recentf" "ido"
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (global-set-key "\C-x\C-r" 'recentf-ido-find-file))

;; unique names
(setup "uniquify"
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;; indentation
(setq-default tab-width 2)
;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
;; makefile has its issues
(add-hook 'makefile-mode-hook 'usetabs)
(add-hook 'python-mode-hook 'usetabs)

;; automatically indent on return, except in a few modes that have similar stuff by default
(electric-indent-mode 1)
;; TODO should be a generic macro or something
(defun no-electric-indent-yaml ()
  (electric-indent-mode -1)
  (define-key yaml-mode-map [(return)] 'newline-and-indent))
(add-hook 'yaml-mode-hook 'no-electric-indent-yaml)
(defun no-electric-indent-python ()
  (electric-indent-mode -1)
  (define-key python-mode-map [(return)] 'newline-and-indent))
(add-hook 'python-mode-hook 'no-electric-indent-python)

;; also indent when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key "\C-y" 'yank-and-indent)

;; undo hardwrapped regions (mostly markdown)
(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs,
indented text (quotes, code) and lists intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))
(global-set-key "\M-Q" 'unfill-region)

;; insert new line *after* the current one
(defun next-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'next-newline-and-indent)

;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(global-set-key "\C-k" 'kill-and-join-forward)

;; delete all space before point up to beginning of line or non-whitespace char
(setup "hungry-delete"
  (global-hungry-delete-mode)
  (defun literal-delete-char (&optional arg)
    (interactive "P")
    (delete-char 1))
  (defun literal-delete-backward-char (&optional arg)
    (interactive "P")
    (delete-backward-char 1))
  (global-set-key (kbd "C-d") 'literal-delete-char)
  (global-set-key (kbd "M-d") 'literal-delete-backward-char))

(autoload 'wcheck-mode "wcheck-mode"
  "Toggle wcheck-mode." t)
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

;; align
(setup "align"
  ;; definitions for ruby code
  ;; fixes the most egregious mistake in detecting regions (hashes), but should be properly generalized at some point
  (setq align-region-separate "\\(^\\s-*[{}]?\\s-*$\\)\\|\\(=\\s-*[][{}()]\\s-*$\\)"))
(setup-expecting "align"
  (setup-after "enh-ruby-mode"
    (defconst align-ruby-modes '(enh-ruby-mode)
      "align-perl-modes is a variable defined in `align.el'.")
    (defconst ruby-align-rules-list
      '((ruby-comma-delimiter
         (regexp . ",\\(\\s-*\\)[^/ \t\n]")
         (modes . '(enh-ruby-mode))
         (repeat . t))
        (ruby-string-after-func
         (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
         (modes . '(enh-ruby-mode))
         (repeat . t))
        (ruby-symbol-after-func
         (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
         (modes . '(enh-ruby-mode))))
      "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details."))
  (add-to-list 'align-perl-modes 'enh-ruby-mode)
  (add-to-list 'align-dq-string-modes 'enh-ruby-mode)
  (add-to-list 'align-sq-string-modes 'enh-ruby-mode)
  (add-to-list 'align-open-comment-modes 'enh-ruby-mode)
  (dolist (it ruby-align-rules-list)
    (add-to-list 'align-rules-list it))
  (setup-after "haskell-mode"
    ;; haskell alignments
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes quote (haskell-mode literate-haskell-mode)))))
  ;; align current region
  (global-set-key (kbd "C-c =") 'align-current)
  ;; repeat regex (teh fuck ain't that the default?!)
  (defun align-repeat (start end regexp)
    "Repeat alignment with respect to the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))
  (global-set-key (kbd "C-c C-=") 'align-repeat))

;; if no region is active, act on current line
(setup "whole-line-or-region"
  (setq whole-line-or-region-extensions-alist
        '((comment-dwim whole-line-or-region-comment-dwim-2 nil)
          (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
          (kill-region whole-line-or-region-kill-region nil)
          (kill-ring-save whole-line-or-region-kill-ring-save nil)
          (yank whole-line-or-region-yank nil)
          ))
  (whole-line-or-region-mode 1))

;; tramp
(setup "tramp"
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name "~/.emacs.d/cache/tramp"))

;; normal bookmarks
(setq bookmark-default-file "~/.emacs.d/cache/bookmarks")

;; undo window changes
(setup "winner"
  (winner-mode 1))

;; number windows, i.e. M-1 .. M-0 to jump to window
(setup "window-numbering"
  (window-numbering-mode 1))

;; ace-jump (hint-style navigation)
(setup "ace-jump-mode"
  (global-set-key (kbd "C-c j") 'ace-jump-mode)
  (global-set-key (kbd "C-c C-g") 'ace-jump-line-mode))

;; expand-region
(setup-lazy '(er/expand-region er/contract-region) "expand-region"
  (global-set-key (kbd "<C-prior>") 'er/expand-region)
  (global-set-key (kbd "<C-next>") 'er/contract-region))

;; make zsh aliases work
(setq shell-command-switch "-lc")

;; scratchpad buffers
(setup "scratch"
  ;; don't want to remember which key I used
  (global-set-key (kbd "C-c b") 'scratch)
  ;; don't start in lisp
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message nil))

;; oh pretty!
(setup "pretty-lambdada"
  (global-pretty-lambda-mode))

;; rotate / toggle text
(setup "rotate-text"
  (global-set-key (kbd "C-c C-t") 'rotate-text)
  (add-to-list 'rotate-text-words '("true" "false"))
  (add-to-list 'rotate-text-words '("yes" "no"))
  (add-to-list 'rotate-text-words '("t" "nil"))
  (add-to-list 'rotate-text-words '("if" "unless"))
  (add-to-list 'rotate-text-words '("map" "each"))
  (add-to-list 'rotate-text-words '("select" "reject"))
  (add-to-list 'rotate-text-symbols '("?" "!")))

;; handle camelcase better
(global-subword-mode 1)

;; folding
(setup "hideshow")
(setup-lazy '(hs-minor-mode) "hideshowvis"
  (add-hook 'enh-ruby-hook   'hs-minor-mode))
(setup-lazy '(hs-minor-mode) "fold-dwim"
  (define-key global-map (kbd "C-c C-f") 'fold-dwim-toggle)
  (define-key global-map (kbd "C-c f")   'fold-dwim-hide-all)
  (define-key global-map (kbd "C-c F") 'fold-dwim-show-all))

;; fast navigation
(setup "imenu"
  ;; recentering
  (setq recenter-positions '(2 middle))
  (add-hook 'imenu-after-jump-hook 'recenter-top-bottom))
(setup "idomenu"
  (define-key global-map (kbd "C-c [") 'idomenu)
  (define-key global-map (kbd "C-c C-[") 'idomenu))
(setup "imenu-anywhere"
  (define-key global-map (kbd "C-c ]") 'imenu-anywhere)
  (define-key global-map (kbd "C-c C-]") 'imenu-anywhere))

;; smartparens
(setup "smartparens-config"
  (smartparens-global-mode t)
  (show-smartparens-global-mode +1)
  (setq sp-highlight-pair-overlay nil)
  (show-smartparens-global-mode t)
  (setq blink-matching-paren-distance nil)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  ;; markdown-mode
  (setup-expecting "markdown-mode" 
    (sp-with-modes '(markdown-mode)
      (sp-local-pair "*" "*" :actions '(wrap autoskip))
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "m" "$" "$") ; math
      (sp-local-pair "$$" "$$" :actions '(wrap autoskip))
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)))
  (setup-expecting "org-mode" 
    (sp-with-modes '(org-mode)
      (sp-local-pair "$" "$" :actions '(wrap autoskip))
      (sp-local-pair "$$" "$$" :actions '(wrap autoskip))))
  ;; html-mode
  (setup-expecting "html-mode" "sgml-mode"
    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))))

(setup-after "smartparens-config"
  ;; sp keys
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-f") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-b") 'sp-backward-symbol)
  (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
  (define-key sp-keymap (kbd "C-<left>") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "C-<right>") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key sp-keymap (kbd "C-c k") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-c C-k") 'sp-rewrap-sexp)
  (define-key sp-keymap (kbd "S-<left>") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "S-<right>") 'sp-select-next-thing)
  (define-key sp-keymap (kbd "C-c |") 'sp-split-sexp)
  (define-key sp-keymap (kbd "C-c C-|") 'sp-join-sexp)

  ;; move to beginning of text on line
  (defun sp-kill-to-end-of-sexp ()
    "Kill everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-end-of-sexp)
      (kill-region (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-kill-to-beginning-of-sexp ()
    "Kill everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-beginning-of-sexp)
      (kill-region (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-copy-to-end-of-sexp ()
    "Copy everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-end-of-sexp)
      (kill-ring-save (mark) (point))))
  ;; move to beginning of text on line
  (defun sp-copy-to-beginning-of-sexp ()
    "copy everything in the sexp without unbalancing it."
    (interactive)
    (save-excursion
      (set-mark (point))
      (sp-beginning-of-sexp)
      (kill-ring-save (mark) (point))))
  (define-key sp-keymap (kbd "C-c a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c e") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-c C-a") 'sp-kill-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c C-e") 'sp-kill-to-end-of-sexp)
  (define-key sp-keymap (kbd "C-c M-a") 'sp-copy-to-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-c M-e") 'sp-copy-to-end-of-sexp))

                                        ; don't use shift to mark things; smartparens overwrites this anyway, but be explicit about it
(setq shift-select-mode nil)

;; transparently open compressed files
(auto-compression-mode t)

;; normally smartparens wraps selected text, but if input is not a pair, just overwrite the text
(delete-selection-mode 1)

;; when popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; automatically create directories if necessary
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; gist
(setup-after "yagist"
  (setup "cipher/aes"
    (setq yagist-encrypt-risky-config t)))

;; More emacs rocks: Join current line with the next
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; go to last change
;; http://www.emacswiki.org/emacs/GotoChg
(setup "goto-chg"
  (global-set-key [f8] 'goto-last-change))

;; Find init file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)

;; Some saner clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

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
