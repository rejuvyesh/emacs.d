;; auto completion

;; snippets
(setup-in-idle "yasnippet")
(setup-lazy '(yas-expand yas-next-field-or-maybe-expand) "yasnippet"
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1)
  (yas-reload-all)
  (defun my-yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun my-yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))
  (define-key yas-minor-mode-map (kbd "TAB") nil) ; auto-complete
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ; auto-complete
  (define-key yas-keymap (kbd "C-j") 'my-yas/goto-start-of-active-field)
  (define-key yas-keymap (kbd "C-e") 'my-yas/goto-end-of-active-field)
  (define-key yas-minor-mode-map (kbd "C-t") 'yas-expand)
  )

;; auto-yasnippet
(setup-after "yasnippet"
  (setup "auto-yasnippet")
  (global-set-key (kbd "C-c ~") 'aya-create)
  (global-set-key (kbd "C-c C-~") 'aya-expand))

;; auto completion
(setup "fuzzy")
(setup-after "fuzzy"
  (setup "auto-complete-config"
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (setq ac-auto-start 1)
    (setq ac-use-menu-map t
          ac-auto-show-menu t
          ac-quick-help-delay 0.5
          ac-use-fuzzy t
          ac-ignore-case nil)
    (setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
    ;; extra modes auto-complete must support
    (global-auto-complete-mode t)
    (define-key ac-completing-map (kbd "RET") 'ac-complete)
    (define-key ac-completing-map (kbd "M-n") 'ac-next)
    (define-key ac-completing-map (kbd "M-p") 'ac-previous)
    ))
(setup-after "auto-complete-config"
  (dolist (mode '(css-mode
                  enh-ruby-mode
                  haml-mode
                  haskell-mode 
                  html-mode
                  js2-mode
                  lisp-mode
                  log-edit-mode
                  markdown-mode
                  matlab-mode
                  org-mode
                  sass-mode
                  sh-mode
                  text-mode
                  textile-mode
                  yaml-mode
                  magit-log-edit-mode))
    ;; Learn emacs list to string so that you can add (setup-expecting mode) before add to list
    ;; (setup-expecting (symbol-name (pop mode))
    ;; (add-to-list 'ac-modes (pop mode))))
    (add-to-list 'ac-modes mode))
  (setq ac-sources '(ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers))
  (setup-expecting "go-mode"
    (setup "go-autocomplete")))

;; auto correction
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq abbrev-mode t)

(provide 'setup-autocomplete)
;;; setup-autocomplete.el ends here
