;; auto completion

;; snippets
(setup "yasnippet"
  (setq yas-snippet-dirs "~/.emacs.d/snippets")
  (define-key yas-minor-mode-map (kbd "C-t") 'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "M-t") 'yas-prev-field)
  (yas-global-mode 1))

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
    (define-key ac-complete-mode-map "\M-n" 'ac-next)
    (define-key ac-complete-mode-map "\M-p" 'ac-previous)))
(setup-after "auto-complete-config"
  (dolist (mode '(org-mode
                  text-mode
                  haml-mode
                  sass-mode
                  yaml-mode
                  haskell-mode 
                  html-mode
                  sh-mode
                  lisp-mode
                  textile-mode
                  markdown-mode
                  js2-mode
                  css-mode
                  matlab-mode
                  enh-ruby-mode))
    ;; Learn emacs list to string so that you can add (setup-expecting mode) before add to list
    ;; (setup-expecting (symbol-name (pop mode))
    ;; (add-to-list 'ac-modes (pop mode))))
    (add-to-list 'ac-modes mode))
  ;; magit-log-edit-mode
  ;; log-edit-mode
  (setup-expecting "go-mode"
    (setup "go-autocomplete")))

;; disabling Yasnippet completion
(setup-after "auto-complete" "yasnippet"
  (defun yasnippet-snips-from-table (table)
    (with-no-warnings
      (let ((hashtab (ac-yasnippet-table-hash table))
            (parent (ac-yasnippet-table-parent table))
            candidates)
        (maphash (lambda (key value)
                   (push key candidates))
                 hashtab)
        (identity candidates)
        )))
  (defun yasnippet-get-all-snips ()
    (let (candidates)
      (maphash
       (lambda (kk vv) (push (yasnippet-snips-from-table vv) candidates)) yas--tables)
      (apply 'append candidates))
    )
  (setq ac-ignores (concatenate 'list ac-ignores (yasnippet-get-all-snips))))

;; auto correction
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq default-abbrev-mode t)

;;; setup-autocomplete.el ends here
