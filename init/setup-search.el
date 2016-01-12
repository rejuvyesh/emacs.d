;; general options
(setq case-fold-search nil)

(require 'kill-ring-search)
(global-set-key (kbd "M-C-y") 'kill-ring-search)
;; cycle in the reverse direction
(defun yank-pop-reverse ()
  (interactive)
  (yank-pop -1))
(global-set-key (kbd "M-Y") 'yank-pop-reverse)

;; goto and hint-style navigation
(require 'ace-jump-mode)
(require 'ace-jump-buffer)
(setq ace-jump-mode-scope 'window)
(global-set-key (kbd "M-g M-g") 'goto-line)
(global-set-key (kbd "M-g b")   'ace-jump-buffer)
(global-set-key (kbd "M-g c")   'ace-jump-char-mode)
(global-set-key (kbd "M-g g")   'ace-jump-mode)
(global-set-key (kbd "M-g l")   'ace-jump-line-mode)

(require 'phi-search)

(require 'visual-regexp)
(require 'visual-regexp-steroids)
(defun vr/query-replace-from-beginning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'vr/query-replace)))
(global-set-key (kbd "C-c r") 'vr/query-replace)
(global-set-key (kbd "C-c R") 'vr/query-replace-from-beginning)


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
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)
(global-set-key (kbd "C-S-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-S-r") 'isearch-backward-regexp)

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
(global-set-key (kbd "C-c *") 'isearch-word-at-point)

;; better grep
(require 'phi-grep)
(setq phi-grep-window-height 40)
(setq phi-grep-make-backup-function nil)

(provide 'setup-search)
