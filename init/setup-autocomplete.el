;; auto completion setup

;; snippets
(use-package yasnippet
  :ensure t
  :init
  ;; set snippet directory
  (setq yas-snippet-dirs (emacs-d "snippets"))
  ;; turn on yasnippet everywhere
  (yas-global-mode 1)
  (yas-reload-all)
  :config
  ;; options
  (setq yas-indent-line 'fixed)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))

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


(define-key yas-keymap (kbd "C-a") 'my-yas/goto-start-of-active-field)
(define-key yas-keymap (kbd "C-e") 'my-yas/goto-end-of-active-field)
(define-key yas-minor-mode-map (kbd "TAB") nil) ; auto-complete uses this
(define-key yas-minor-mode-map (kbd "<tab>") nil) ; auto-complete uses this
;; Use C-t to expand snippet instead of conflicting <TAB>
(define-key yas-minor-mode-map (kbd "C-t") 'yas-expand)
(define-key yas-keymap (kbd "C-t") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "C-T") 'yas-next-field)
(define-key yas-minor-mode-map (kbd "C-c C-t") 'yas-reload-all)



;; auto-yasnippet
;; hybrid of keyboard macro and yasnippet
(use-package auto-yasnippet
  :ensure t
  :bind (("C-c ~" . aya-create)
         ("C-c C-~" . aya-expand)))


;; auto completion
;;(use-package fuzzy)

(use-package auto-complete-config)
    ;; set sources to look into
(setq ac-sources '(ac-source-abbrev
                   ac-source-dictionary
                   ac-source-words-in-same-mode-buffers))
(add-to-list 'ac-dictionary-directories (emacs-d "ac-dict"))
(setq ac-comphist-file (emacs-d "cache/ac-comphist.dat"))
;; auto start completion
(setq ac-auto-start 1)
(setq ac-use-menu-map t  
      ac-auto-show-menu t
      ac-quick-help-delay 0.5
  ;;    ac-use-fuzzy t
      ac-ignore-case nil)
;; To get pop-ups with docs even if a word is uniquely completed
(setq ac-dwim nil) 
;; use autocomplete evrywhere
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map (kbd "M-n") 'ac-next)
(define-key ac-completing-map (kbd "M-p") 'ac-previous)
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
      ;; Learn emacs list to string so that you can add (require-xpecting mode) before add to list
      ;; (require-xpecting (symbol-name (pop mode))
      ;; (add-to-list 'ac-modes (pop mode))))
  (add-to-list 'ac-modes mode))

(load-after 'go-mode
  (use-package go-autocomplete))

(use-package ac-math)
(defvar ac-source-math-latex-everywhere
  '((candidates . ac-math-symbols-latex)
    (prefix . "\\\\\\(.*\\)")
    (action . ac-math-action-latex)
    (symbol . "l")))
(load-after 'ac-math
            (defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
              (setq ac-sources
                    (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                            ac-sources)))
            (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
            (add-hook 'org-mode-hook
                      (lambda()
                        (add-to-list 'ac-sources 'ac-source-math-unicode)
                        (add-to-list 'ac-sources 'ac-source-math-latex-everywhere)))
            (add-hook 'markdown-mode-hook
                      (lambda()
                        (add-to-list 'ac-sources 'ac-source-math-latex-everywhere)))
            )


;; auto correction via abbreviation file
(load-after 'abbrev
            (setq abbrev-file-name
                  (emacs-d "abbrev_defs"))
            (setq save-abbrevs 'silently)
            (when (file-exists-p abbrev-file-name)
              (quietly-read-abbrev-file))
            )

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
(setq-default abbrev-mode t)

(provide 'setup-autocomplete)
;;; setup-autocomplete.el ends here
