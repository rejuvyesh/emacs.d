;; looks

;; prettier defaults
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
;; mostly for font-lock speed issues
(setq jit-lock-stealth-time 5)
(setq frame-title-format "%b")
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)
(set-fringe-mode '(1 . 10))

;; selective hooks for either terminals or X windows
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")

(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(defun run-after-make-frame-hooks-current-frame ()
  (run-after-make-frame-hooks (selected-frame)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
(add-hook 'after-init-hook 'run-after-make-frame-hooks-current-frame)

;; color themes
(add-to-list 'custom-theme-load-path (emacs-d "themes/"))

(defvar bright-theme	'leuven              	"Bright theme to use")
(defvar dark-theme  	'twilight-anti-bright	"Dark theme to use")

(defvar use-bright-theme nil "Whether to use the bright or dark theme")


(defun load-correct-theme ()
  "Loads appropriate theme."
  (interactive)
  (if use-bright-theme (load-theme bright-theme t)
    (load-theme dark-theme t)))

(defun load-theme-as-time()
  "Check time and change theme setting"
  (let*
      ((now (current-time))
       (today-times    (sunrise-sunset-times (today)))
       (tomorrow-times (sunrise-sunset-times (tomorrow)))
       (sunrise-today (first today-times))
       (sunset-today (second today-times))
       (sunrise-tomorrow (first tomorrow-times)))
    (if (daytime-p sunrise-today sunset-today)
        (progn
          (setq use-bright-theme t)
          (disable-theme dark-theme)
          (load-correct-theme)
          (run-at-time (+second sunset-today) nil
                       'load-theme-as-time))
      (setq use-bright-theme nil)
      (disable-theme bright-theme)
      (load-correct-theme)
      (if (time-less-p now sunrise-today)
          (run-at-time (+second sunrise-today) nil
                       'load-theme-as-time)
        (run-at-time (+second sunrise-tomorrow) nil
                     'load-theme-as-time)))))

(when (pretty-load?)
  (load-theme-as-time))

(defun toggle-bright-theme ()
  "toggles between bright and dark theme"
  (interactive)
  (if use-bright-theme (progn
                         (setq use-bright-theme nil)
                         (disable-theme bright-theme)
                         (load-theme dark-theme t))
    (progn
      (setq use-bright-theme t)
      (disable-theme dark-theme)
      (load-theme bright-theme t))))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package hl-line)

(use-package nyan-mode
  :init
  (nyan-mode t))

;; fonts
(defvar small-font  "Fantasque Sans Mono 8")
(defvar normal-font "Fantasque Sans Mono 10")
(defvar big-font    "Fantasque Sans Mono 11")
(defvar huge-font   "Fantasque Sans Mono 13")
(defvar font-list '(small-font
                    normal-font
                    big-font
                    huge-font))
(defvar default-font normal-font)

(defun set-window-font (&optional font)
  (set-frame-font (or font default-font)))

(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

(when (pretty-load?)
  (set-window-font))

;; shortcut for the fonts
(defmacro use-font (font)
  `(defun ,(intern (format "use-%s" font)) ()
     ,(format "Use font set in '%s'" font)
     (interactive)
     (set-window-font ,font)))

(loop for font in font-list collect (eval `(use-font, font)))

;; scrolling
(setq scroll-preserve-screen-position t)
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; move pointer to the farthest possible position
(setq scroll-error-top-bottom t)
;; smooth scrolling with margin
;; necessary or scrolling is really slow
(setq-default bidi-display-reordering nil)
(setq auto-window-vscroll nil)
(setq scroll-margin 5)
(setq scroll-conservatively 10000)

;; optical stuff
;; Stop cursor blinking
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

;; undo highlighting
;; highlight changes made by undo
(use-package volatile-highlights 
             :ensure t
             :config
             (volatile-highlights-mode t))

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; avoid splitting horizontally
;; because all screens these days are widescreen
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; show #colors in matching color
(use-package rainbow-mode
             :ensure t)
(defadvice rainbow-mode (after rainbow-mode-refresh activate)
  (font-lock-fontify-buffer))


;; highlight some whitespace
(use-package leerzeichen
  :commands (leerzeichen-mode))
(add-hook 'prog-mode-hook  'leerzeichen-mode)
(add-hook 'dired-mode-hook 'leerzeichen-mode)

;; parenthesis highlighting behavior
(use-package paren
  :config
  (setq blink-matching-paren-distance nil)
  (setq show-paren-style 'expression)
  (setq show-paren-delay 0.05) ; don't start highlighting when just scrolling past
  (show-paren-mode 1))

(defun toggle-line-wrap ()
  (interactive)
  (if truncate-lines
      (toggle-truncate-lines 1)
    (toggle-truncate-lines 0))
  (if visual-line-mode
      (visual-line-mode 0)
    (visual-line-mode 1)))

;; make regexpes a bit more readable by default
(defun fontify-glyph (item glyph)
  `((,item
     (0 font-lock-preprocessor-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph) nil)))))

(font-lock-add-keywords 'emacs-lisp-mode (fontify-glyph "\\\\\\\\" "\\"))

;; Pretty mode
;; Base set of pretty symbols.
(defvar base-prettify-symbols-alist '(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("<-" . ?←)
                                      ("->" . ?→)
                                      ("lambda" . ?λ)))
(defun prettify-symbols-hook ()
  "Set pretty symbols for programming modes."
  (setq prettify-symbols-alist
        (append '(("==" . ?≡)
                  ("!=" . ?≠)) base-prettify-symbols-alist)))
(add-hook 'prog-mode-hook 'prettify-symbols-hook)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-hook)
(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; don't hard-wrap text, but use nice virtual wrapping
(use-package adaptive-wrap)
(setq-default fill-column 80)
(global-adaptive-wrap-prefix-mode 1)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
;; don't wrap lines by default
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)




(use-package diminish
  :config
  ;; diminish
  ;; hide information about minor modes from mode-line
  ;; clean up modeline and hide standard minor modes
  (defmacro diminish-minor-mode (package mode &optional short-name)
    `(load-after ,package
       (when (fboundp ,mode)
	 (diminish ,mode ,(or short-name "")))))
  ;; clean up way-too-long major modes
  (defmacro diminish-major-mode (package-name mode new-name)
    `(load-after ,package-name
       '(defadvice ,mode (after diminish-major-mode activate)
	  (setq mode-name ,new-name))))
  (diminish-minor-mode 'abbrev               'abbrev-mode)
  (diminish-minor-mode 'company              'company-mode       "↝")
  (diminish-minor-mode 'auto-revert-mode     'auto-revert-mode)
  (diminish-minor-mode 'eldoc                'eldoc-mode)
  (diminish-minor-mode 'fic-mode             'fic-mode)
  (diminish-minor-mode 'haskell-doc          'haskell-doc-mode)
  (diminish-minor-mode 'hs-minor-mode        'hs-minor-mode)
  (diminish-minor-mode 'leerzeichen          'leerzeichen-mode)
  (diminish-minor-mode 'magit                'magit-auto-revert-mode)
  (diminish-minor-mode 'smartparens          'smartparens-mode)
  (diminish-minor-mode 'undo-tree            'undo-tree-mode     "↺")
  (diminish-minor-mode 'visual-line-mode     'visual-line-mode)
  (diminish-minor-mode 'volatile-highlights  'volatile-highlights-mode)
  (diminish-minor-mode 'whole-line-or-region 'whole-line-or-region-mode)
  (diminish-minor-mode 'yasnippet            'yas-minor-mode)

  (diminish-major-mode	'lisp-mode    	emacs-lisp-mode	"EL" 	)
  (diminish-major-mode	'sh-script    	sh-mode        	"sh" 	)
  (diminish-major-mode	'python-mode  	python-mode    	"PY" 	)
  (diminish-major-mode	'ruby-mode    	ruby-mode      	"RB" 	)
  (diminish-major-mode	'enh-ruby-mode	enh-ruby-mode  	"RB+"	))

(provide 'setup-look)
;;; setup-look ends here
