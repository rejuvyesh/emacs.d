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

;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Remove background color from terminal emacs,
;; so that it can remain transparent
;; http://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
;; (defun on-frame-open (frame)
;;   (if (not (display-graphic-p frame))
;;       (set-face-background 'default "unspecified-bg" frame)))
;; (on-frame-open (selected-frame))
;; (add-hook 'after-make-frame-functions 'on-frame-open)

(defvar bright-theme 'leuven  "Bright theme to use")
(defvar dark-theme   'molokai "Dark theme to use")

(defvar use-bright-theme t "Whether to use the bright or dark theme")

(defun load-correct-theme ()
  "Loads appropriate theme."
  (interactive)
  (if use-bright-theme (load-theme bright-theme t)
    (load-theme dark-theme t))
  )

(when (pretty-load?)
  (load-correct-theme))

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

;; highlight current line
(defface hl-line '((t (:background nil)))
  "Face to use for `hl-line-face'." :group 'hl-line)

(setup "hl-line"
  (setq hl-line-face 'hl-line)
  (global-hl-line-mode t))

;; fonts
(defvar small-font  "Consolas 8")
(defvar normal-font "Consolas 11")
(defvar big-font    "Consolas 14")
(defvar font-list (list
                   small-font
                   normal-font
                   big-font))
(defvar current-font normal-font)

(defun set-window-font ()
  (set-frame-font current-font))

(when (pretty-load?)
  (set-window-font))

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

;; scrolling
(setq scroll-preserve-screen-position t)
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; move pointer to the farthest possible position
(setq scroll-error-top-bottom t)
;; smooth scrolling with margin
(setup "smooth-scrolling"
  (setq smooth-scroll-margin 5)
  (setq scroll-margin 0)
  (setq scroll-conservatively 10000)
  ;; necessary or scrolling is really slow
  (setq-default bidi-display-reordering nil)
  (setq auto-window-vscroll nil))

;; ;; yascroll
;; (global-yascroll-bar-mode 1)

;; try to keep windows within a max margin
(setup "automargin"
  (setq automargin-target-width 120)
  (when (pretty-load?)
    (automargin-mode)))

;; smart-mode line
;; fix mode line with colors
(setup "smart-mode-line"
  (setq sml/theme 'dark)
  (sml/setup))

;; optical stuff
;; (setup "heartbeat-cursor"
;;   (heartbeat-cursor-mode))
;; Stop cursor blinking
(blink-cursor-mode -1)
(setq-default cursor-type 'box)

;; undo highlighting
;; highlight changes made by undo
(setup "volatile-highlights"
  (volatile-highlights-mode t))

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; avoid splitting horizontally
;; because all screens these days are widescreen
(setq split-height-threshold nil)
(setq split-width-threshold 90)

;; show #colors in matching color
(setup-lazy '(rainbow-mode) "rainbow-mode")

;; diminish
;; hide information about minor modes from mode-line
(setup "diminish"
  (setup-after "abbrev"                (diminish 'abbrev-mode))
  (setup-after "anzu"                  (diminish 'anzu-mode))
  (setup-after "auto-complete"         (diminish 'auto-complete-mode "↝"))
  (setup-after "auto-revert-mode"      (diminish 'auto-revert-mode))
  (setup-after "eldoc"                 (diminish 'eldoc-mode))
  (setup-after "fic-mode"              (diminish 'fic-mode))
  (setup-after "guide-key"             (diminish 'guide-key-mode))
  (setup-after "haskell-doc"           (diminish 'haskell-doc-mode))
  (setup-after "haskell-indentation"   (diminish 'haskell-indentation-mode))
  (setup-after "highlight-parentheses" (diminish 'highlight-parentheses-mode))
  (setup-after "hs-minor-mode"         (diminish 'hs-minor-mode))
  (setup-after "smartparens"           (diminish 'smartparens-mode))
  (setup-after "undo-tree"             (diminish 'undo-tree-mode "↺"))
  (setup-after "visual-line-mode"      (diminish 'visual-line-mode))
  (setup-after "volatile-highlights"   (diminish 'volatile-highlights-mode))
  (setup-after "whole-line-or-region"  (diminish 'whole-line-or-region-mode))
  (setup-after "yasnippet"             (diminish 'yas-minor-mode)))

(provide 'setup-look)
