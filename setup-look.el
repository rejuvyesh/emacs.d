;; color themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Remove background color from terminal emacs,
;; so that it can remain transparent
;; http://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

;; using modified molokai theme
(load-theme 'molokai t)

;; fonts
(defvar small-font "Terminus 8")
(defvar normal-font "Consolas 10")
(defvar big-font "Consolas 14")
(defvar font-list (list
                   small-font
                   normal-font
                   big-font))
(defvar current-font normal-font)

(defun set-window-font ()
  (set-frame-font current-font))
(add-hook 'after-make-window-system-frame-hooks 'set-window-font)

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
(setq mouse-wheel-progressive-speed nil)
(setq scroll-error-top-bottom t)
;; smooth scrolling with margin
(setup "smooth-scrolling"
  (setq smooth-scroll-margin 5)
  (setq scroll-margin 0)
  (setq scroll-conservatively 10000)
  ;; necessary or scrolling is really slow
  (setq-default bidi-display-reordering nil)
  (setq auto-window-vscroll nil))
;; yascroll
(global-yascroll-bar-mode 1)

;; try to keep windows within a max margin
(setup "automargin"
  (setq automargin-target-width 120)
  (automargin-mode))

;; highlight current line
(defface hl-line '((t (:background "aquagreen")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
(global-hl-line-mode t)

;; smart-mode
(setup "smart-mode-line"
  (setq sml/theme 'dark)
  (sml/setup))

;; remove the toolbar which no-one uses :)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; optical stuff
(setup "heartbeat-cursor"
  (heartbeat-cursor-mode))
(blink-cursor-mode -1)
(setq-default cursor-type 'box)
(setq inhibit-splash-screen t)

;; shows current selected region
(setq-default transient-mark-mode t)
(global-font-lock-mode t)
(setq jit-lock-stealth-time 5)
(setq frame-title-format "%b")
(set-fringe-mode '(1 . 10))

;; undo highlighting
(setup "volatile-highlights"
  (volatile-highlights-mode t))

;; show #colors in matching color
(setup "rainbow-mode")

;; ag mode
(setq ag-highlight-search t)

;; diminish
(setup "diminish"
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
  (setup-after "smartparens-autoloads" (diminish 'smartparens-mode))
  (setup-after "undo-tree"             (diminish 'undo-tree-mode "↺"))
  (setup-after "visual-line-mode"      (diminish 'visual-line-mode))
  (setup-after "volatile-highlights"   (diminish 'volatile-highlights-mode))
  (setup-after "whole-line-or-region"  (diminish 'whole-line-or-region-mode))
  (setup-after "yasnippet"             (diminish 'yas-minor-mode)))

(provide 'setup-look)
