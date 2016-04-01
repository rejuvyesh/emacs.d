;; generic editing features

;; safety first
;; save all auto saves in a single directory
(setq make-backup-files nil)
(defvar autosave-dir (expand-file-name (emacs-d "cache/autosave-dir/")))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq confirm-kill-emacs 'y-or-n-p)
;; move files to trash instead
(setq delete-by-moving-to-trash t)


;; save location inside buffer
(use-package saveplace)
;; save minibuffer history
(savehist-mode 1)
;; more useful kill-ring
(setq kill-ring-max          1000)
(setq history-length         1000)
(setq search-ring-max        1000)
(setq regexp-search-ring-max 1000)
(setq savehist-additional-variables '(search-ring
                                      regexp-search-ring
                                      kill-ring
                                      compile-command))
(setq savehist-file   (emacs-d "cache/history"))
(setq save-place-file (emacs-d "cache/saveplace"))
(setq-default save-place t)


;; text stuff
(setq undo-limit 1000000)
(setq sentence-end-double-space nil)
(column-number-mode t)
(setq-default indicate-empty-lines t)

(setq locale-coding-system  'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

;; enable useful commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; general key bindings

;; commenting
(bind-key "C-c c"     'comment-region)
(bind-key "C-c u"     'uncomment-region)
(bind-key "C-c SPC"   'comment-dwim)
(bind-key "C-c C-SPC" 'comment-dwim)

;; error
(bind-key "C-c n" 'next-error)
(bind-key "C-c p" 'previous-error)
(bind-key "M-t"   'find-tag)

;; eval shortcut
(bind-key "C-S-M-x"   'eval-buffer)


(bind-key "C-f"    'forward-word)
(bind-key "C-b"    'backward-word)
(bind-key "M-f"    'forward-sentence)
(bind-key "M-b"    'backward-sentence)
(bind-key "<home>" 'beginning-of-buffer)
(bind-key "<end>"  'end-of-buffer)

;; allowed key components
(use-package free-keys
  :config
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

(use-package multiple-cursors
  :ensure t
  :bind (("C-c d"            . mc/edit-lines)
         ("<C-down>"         . mc/mark-next-like-this)
         ("<C-up>"           . mc/mark-previous-like-this)
         ("<M-C-down>"       . mc/skip-to-next-like-this)
         ("<M-C-up>"         . mc/skip-to-previous-like-this)
         ("C-c C-d"          . mc/mark-all-dwim)
         ("C-c >"            . mc/mark-more-like-this-extended)
         ("C-c <"            . mc/mark-more-like-this-extended)
         ("C-S-<mouse-1>"    . mc/add-cursor-on-click)
         ("C-<down-mouse-1>" . mc/add-cursor-on-click)
         (:map mc/keymap
               ;; <ret> inserts a newline; C-j exits (a bit more convenient that way)
               ("<return>" . nil)
               ("C-j"      . multiple-cursors-mode)))
  :config
  (defun mc/many-to-one-yank ()
    "Yanks killed lines from multiple cursors into one position. Less messy than yank-rectangle."
    (interactive)
    (with-temp-buffer
      (yank-rectangle)
      (kill-ring-save (point-min) (point-max)))
    (yank))

  (defun mc/many-to-one-yank-indent ()
    "Yanks killed lines from multiple cursors into one position, and indents. See 'mc/many-to-one-yank'."
    (interactive)
    (mc/many-to-one-yank)
    (call-interactively 'indent-region))

  (use-package mc-extras
    :ensure t)
  (use-package mc-jump)

  ;; Because regex
  (defalias 'mc/mark-all-in-region 'mc/mark-all-in-region-regexp)
  )

(use-package phi-search-mc
  :ensure t
  :defer t
  :bind (:map phi-search-default-map
              ("C-<down>" . phi-search-mc/mark-next)
              ("C-<up>"   . phi-search-mc/mark-previous)
              ("C-c C-k"  . phi-search-mc/mark-all)))

;; undo tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :commands (undo-tree-undo undo-tree-redo)
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-timestamps t)

  ;; undo
  :bind (("C-z" . undo-tree-undo)
         ("M-z" . undo-tree-redo)
         (:map undo-tree-map
               ("C-x u" . undo-tree-visualize)))
  )


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
(bind-key "M-k" 'copy-line)

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
(bind-key "C-a" 'smart-beginning-of-line)

(defun smart-end-of-line ()
  "Move point to end of visual line or, if already there, to end of logical line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))

    (end-of-visual-line)
    (when (= oldpos (point))
      (end-of-line))))
(bind-key "C-e" 'smart-end-of-line)

;; org-mode has similar behavior built-in, so use it instead
(load-after 'org-mode
  (setq org-special-ctrl-a/e t))

;; move lines like in org-mode
(use-package move-dup
  :bind (("M-<up>" . md/move-lines-up)
         ("M-<down>" . md/move-lines-down)))

(defun insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

Prefixed with \\[universal-argument], expand the file name to its full path."
  (interactive "*fInsert file name: \nP")
  (cond ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert (file-relative-name filename)))))
(bind-key "C-c C-i" 'insert-file-name)


(use-package winner
  :config
  (add-to-list 'winner-boring-buffers "*helm M-x*")
  (add-to-list 'winner-boring-buffers "*helm mini*")
  (winner-mode 1))

(use-package flycheck-proselint
  :defer t
  :after (flycheck))

(use-package flycheck-julia
  :defer t
  :after (flycheck))

;; recent files
(use-package recentf
  :defer t
  :config
  (setq recentf-max-saved-items 500)
  (setq recentf-save-file (emacs-d "cache/recentf"))
  (setq recentf-exclude (append recentf-exclude
                                '("\.emacs\.d/cache"
                                  "\.emacs\.d/elpa")))
  (setq recentf-keep '(file-remote-p file-readable-p))
  (when (not noninteractive) (recentf-mode 1))   ; fix load times
  )

;; clean up buffers every once in a while
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay 0))

;; unique names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)   ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )


;; indentation
(setq-default tab-width 2)
(bind-key "C-c i" 'indent-region)

;; don't use tabs normally, except for a few special modes
(setq-default indent-tabs-mode nil)
(defun use-tabs () (setq indent-tabs-mode t))
;; makefile has its issues
(add-hook 'makefile-mode-hook 'usetabs)
;; insert literal tab
(bind-key "<C-tab>" (lambda () (interactive)
                                  (insert "\t")))


(use-package electric                   ; automatically indent on return
  :config
  (electric-indent-mode 1))

(defadvice electric-indent-post-self-insert-function (around keep-trailing-whitespace activate)
  (noflet ((delete-horizontal-space (&rest args) t))
    ad-do-it))

;; also indent when yanked
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(bind-key "C-y" 'yank-and-indent)

;; undo hardwrapped regions (mostly markdown)
(defun unfill-region (begin end)
  "Remove all line breaks in a region but leave paragraphs,
indented text (quotes, code) and lists intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\\>-\n]\\)" "\\1 \\2" nil begin end))
(bind-key "M-Q" 'unfill-region)

;; delete spaces when killing a line
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))
(bind-key "C-k" 'kill-and-join-forward)

(use-package hungry-delete              ; delete all space before point up to beginning of line or non-whitespace char
  :ensure t
  :diminish (hungry-delete-mode)
  :init
  (global-hungry-delete-mode))

(defun literal-delete-char (&optional arg)
  (interactive "P")
  (delete-char 1))
(defun literal-delete-backward-char (&optional arg)
  (interactive "P")
  (delete-backward-char 1))
(bind-key "C-d" 'literal-delete-char)
(bind-key "M-d" 'literal-delete-backward-char)

(use-package wcheck-mode
  :ensure t
  :bind (("C-c s" . wcheck-actions)))
(setq ispell-really-hunspell t)
(setq wcheck-timer-idle .2)
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
(add-hook 'text-mode-hook     'turn-on-spell-check)
(add-hook 'markdown-mode-hook 'turn-on-spell-check)
(add-hook 'org-mode-hook      'turn-on-spell-check)
(add-hook 'text-mode-hook     'turn-on-spell-check)

;; diff- mode (better colors)
(use-package diff-mode-)
;; a slightly saner diff command
(use-package ediff
  :config
  (setq ediff-diff-options "-w"))


;; if no region is active, act on current line
(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :ensure t
  :init
  (whole-line-or-region-mode 1)
  :config
  (setq whole-line-or-region-extensions-alist
        '((comment-dwim whole-line-or-region-comment-dwim-2 nil)
          (copy-region-as-kill whole-line-or-region-copy-region-as-kill nil)
          (kill-region whole-line-or-region-kill-region nil)
          (kill-ring-save whole-line-or-region-kill-ring-save nil)
          (yank whole-line-or-region-yank nil)
          )))

(use-package whitespace-cleanup
  :commands (whitespace-cleanup whitespace-cleanup-region)
  :bind (("C-. w" . whitespace-cleanup))
  )

;; tramp
(use-package tramp
  :defer t
  :config
  (use-package password-cache
    :init (setq password-cache-expiry nil))
  (setq tramp-default-method "ssh")
  (setq tramp-persistency-file-name (emacs-d "cache/tramp"))
  )

;; cookies
(setq url-cookie-file (emacs-d "cache/url/cookies"))

;; normal bookmarks
(setq bookmark-default-file (emacs-d "cache/bookmarks"))

;; undo window changes
(use-package winner
  :ensure t
  :defer t)

;; number windows, i.e. M-1 .. M-0 to jump to window
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode 1))

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-<prior>" . er/expand-region)
         ( "C-<next>" . er/contract-region)))

;; make zsh aliases work
(use-package shell-command
  :ensure t
  :config
  (setq shell-command-switch "-lc")
  (shell-command-completion-mode))

;; scratchpad buffers
(use-package scratch-ext
  :ensure t
  :bind ("C-c b" . scratch)
  :config
  (setq scratch-ext-log-directory (emacs-d "scratch/")))

;; don't spam *Scratch*
(setq initial-scratch-message nil)

;; handle camelcase better
(use-package subword
  :diminish (subword-mode)
  :config
  (global-subword-mode 1))


;; fast navigation
(use-package imenu
  :config
  (set-default 'imenu-auto-rescan t)
  ;; recentering
  (setq recenter-positions '(2 middle)))

(add-hook 'imenu-after-jump-hook 'recenter-top-bottom)

;; smartparens
(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . " ⓟ")
  :init
  (use-package smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :config
  (setq sp-highlight-pair-overlay nil)
  (setq blink-matching-paren-distance nil)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  ;; sp keys
  (bind-keys :map smartparens-mode-map
             ("C-M-f"         . sp-forward-sexp)
             ("C-M-b"         . sp-backward-sexp)
             ("M-f"           . sp-forward-symbol)
             ("M-b"           . sp-backward-symbol)
             ("C-S-a"         . sp-beginning-of-sexp)
             ("C-S-e"         . sp-end-of-sexp)
             ("C-M-t"         . sp-transpose-sexp)
             ("C-M-n"         . sp-next-sexp)
             ("C-M-p"         . sp-previous-sexp)
             ("C-M-k"         . sp-kill-sexp)
             ("C-M-w"         . sp-copy-sexp)
             ("C-<left>"      . sp-add-to-next-sexp)
             ("C-<right>"     . sp-add-to-previous-sexp)
             ("M-<delete>"    . sp-unwrap-sexp)
             ("M-<backspace>" . sp-backward-unwrap-sexp)
             ("C-c k"         . sp-splice-sexp)
             ("C-c C-k"       . sp-rewrap-sexp)
             ("S-<left>"      . sp-select-previous-thing)
             ("S-<right>"     . sp-select-next-thing)
             ("C-c |"         . sp-split-sexp)
             ("C-c C-|"       . sp-join-sexp)
             ("C-c C-a"       . sp-kill-to-beginning-of-sexp)
             ("C-c C-e"       . sp-kill-to-end-of-sexp)
             ("C-c M-a"       . sp-copy-to-beginning-of-sexp)
             ("C-c M-e"       . sp-copy-to-end-of-sexp))

  ;; markdown-mode
  (sp-with-modes '(markdown-mode)
    (sp-local-pair "*" "*"
                   :wrap "C-*"
                   :unless '(sp-point-after-word-p sp-point-at-bol-p))
    (sp-local-pair "_" "_"
                   :wrap "C-_"
                   :unless '(sp-point-after-word-p))
    (sp-local-pair "**" "**")
    (sp-local-pair "$" "$") ; math
    (sp-local-pair "$$" "$$")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
  ;; org-mode
  (sp-with-modes '(org-mode)
    (sp-local-pair "$" "$" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "$$" "$$")
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC"))))
  ;; html-mode
  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">"))
  ;; C++
  (sp-with-modes '(malabar-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))
  )

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


(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package toggle-quotes
  :ensure t
  :commands (toggle-quotes)
  :bind ("C-'" . toggle-quotes))

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-mode t)
  (wrap-region-add-wrapper "$" "$")
  )

;; don't use shift to mark things; smartparens overwrites this anyway, but be explicit about it
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
(use-package gist
  :ensure t
  :commands (gist-list gist-buffer gist-region)
  :config
  (setq gist-view-gist t))


;; go to last change
;; http://www.emacswiki.org/emacs/GotoChg
(use-package goto-last-change
  :ensure t)
(bind-key "<f8>" 'goto-last-change)

;; Some saner clipboard
(if (>= emacs-major-version 25)
    (setq select-enable-clipboard t)
  (setq x-select-enable-clipboard t))
(if (>= emacs-major-version 25)
     (setq select-enable-primary t)
     (setq x-select-enable-primary t))
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; On saving, automatically make a file an executable if it begins with "#!"
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (setq keyfreq-file (emacs-d "cache/keyfreq"))
  (setq keyfreq-file-lock (emacs-d "cache/keyfreq.lock"))
  (defadvice keyfreq-mode (after always-autosave activate)
    (keyfreq-autosave-mode 1))
  (defun my/keyfreq-save-html ()
    "Save the table of frequently used commands (and their associated bindings
to an html file in `user-emacs-directory'."
    (interactive)
    (keyfreq-html (emacs-d "cache/keyfreq.html"))))

(use-package keyboard-cat-mode
  :commands (keyboard-cat-mode))

(provide 'setup-editing)
