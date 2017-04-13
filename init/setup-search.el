;; general options
(setq case-fold-search nil)

(use-package kill-ring-search
  :ensure t
  :bind (("M-C-y" . kill-ring-search)))

;; cycle in the reverse direction
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(bind-key  "M-Y" 'yank-pop-reverse)

;; goto and hint-style navigation
(use-package avy
  :ensure t
  :bind (("M-g l" . avy-goto-line)
         ("M-g c" . avy-goto-char)
         ("M-g w" . avy-goto-word-or-subword-1)))
(bind-key "M-g M-g" 'goto-line)
(bind-key "M-g g" 'goto-line)


(use-package phi-search
  :ensure t
  :bind (("C-c C-s" . phi-search)
         ("C-c C-r" . phi-search-backward))
  )

(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
    :ensure t)
  (defun vr/query-replace-from-beginning ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (call-interactively 'vr/query-replace)))
  :bind (("C-c r" . vr/query-replace)
         ("C-c R" . vr/query-replace-from-beginning)))


;; use regexp search and selected region (if any) by default
(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward-regexp))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'regexp-search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward-regexp))

;; use regexp search by default
(bind-key "C-s"   'isearch-forward-use-region)
(bind-key "C-r"   'isearch-backward-use-region)
(bind-key "C-S-s" 'isearch-forward-regexp)
(bind-key "C-S-r" 'isearch-backward-regexp)

;; make backspace sane
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-del-char)

(define-key isearch-mode-map (kbd "C-c C-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-c C-r") 'isearch-toggle-regexp)
(define-key isearch-mode-map (kbd "C-c C-i") 'isearch-toggle-case-fold)
(define-key isearch-mode-map (kbd "C-c C-s") 'isearch-toggle-symbol)
(define-key isearch-mode-map (kbd "C-c C-SPC") 'isearch-toggle-lax-whitespace)
(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

;; search word at point, like vim
(defun isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun isearch-yank-word-hook ()
  (when (equal this-command 'isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'isearch-yank-word-hook)
(bind-key "C-c *" 'isearch-word-at-point)

;; better grep
(use-package phi-grep
  :ensure t
  :config
  (setq phi-grep-window-height 40)
  (setq phi-grep-make-backup-function nil))

(use-package ag                         ; ag search
  :if (executable-find "ag")
  :ensure t
  :commands (ag helm-do-grep-ag)
  :config
  (setq ag-highlight-search t)
  )

(use-package wgrep-ag                   ; Wgrep for ag
  :if (executable-find "ag")
  :ensure t
  :defer t
  :config
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))

(use-package ripgrep
  :if (executable-find "rg")
  :ensure t
  :commands (projectile-ripgrep ripgrep-regexp)
  :config
  )


(provide 'setup-search)
