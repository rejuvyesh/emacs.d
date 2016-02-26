;; auto completion setup

;; snippets
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
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
  (setq yas-wrap-around-region t)
  (define-key yas-keymap (kbd "C-a") 'my-yas/goto-start-of-active-field)
  (define-key yas-keymap (kbd "C-e") 'my-yas/goto-end-of-active-field)
  (define-key yas-minor-mode-map (kbd "TAB") nil) ; auto-complete uses this
  (define-key yas-minor-mode-map (kbd "<tab>") nil) ; auto-complete uses this
  ;; Use C-t to expand snippet instead of conflicting <TAB>
  (define-key yas-minor-mode-map (kbd "C-t") 'yas-expand)
  (define-key yas-keymap (kbd "C-t") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "C-T") 'yas-next-field)
  (define-key yas-minor-mode-map (kbd "C-c C-y") 'yas-reload-all))

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

;; auto-yasnippet
;; hybrid of keyboard macro and yasnippet
(use-package auto-yasnippet
  :ensure t
  :commands (aya-create aya-expand)
  :bind (("C-c ~" . aya-create)
         ("C-c C-~" . aya-expand)))


(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.1
        ;; min prefix of 1 chars
        company-minimum-prefix-length 1)
  ;; math
  (use-package company-math
    :ensure t
    :defer t
    :config
    (defun enable-math()
      (setq-local company-backends
                  (append '((company-math-symbols-latex company-latex-commands))
                          company-backends)))
    (add-hook 'text-mode-hook 'enable-math)
    (eval-after-load 'auctex
      '(add-hook 'LaTeX-mode-hook 'enable-math)))
  )

(use-package company-quickhelp          ; Show help in tooltip
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-quickhelp-mode)))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (company-statistics-mode)))

(use-package company-emoji              ; Emojis completion like Github/Slack
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-emoji)))

(use-package company-jedi
  :ensure t
  :defer t
  :init
  (defun enable-jedi()
    (setq-local company-backends
                (append '(company-jedi) company-backends)))
  (with-eval-after-load 'company
    (add-hook 'python-mode-hook 'enable-jedi)))

;; auto correction via abbreviation file
(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name
        (emacs-d "abbrev_defs"))
  (setq save-abbrevs 'silently)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (setq-default abbrev-mode t))


(provide 'setup-autocomplete)
;;; setup-autocomplete.el ends here
