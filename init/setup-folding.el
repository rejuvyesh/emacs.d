(use-package hideshow)
(use-package hideshowvis)
(use-package fold-dwim)

(add-hook 'prog-mode-hook 	'hs-minor-mode)

(setq hs-isearch-open t)

;; better ellipsis
(defvar folding-ellipsis "…" "replacement for folded content")

(set-display-table-slot standard-display-table
                        'selective-display
                        folding-ellipsis)
(setq hs-set-up-overlay
      (fn (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display folding-ellipsis))))

;; better buffer folding
(defvar hs-fold-level 1 "folding level")
(make-variable-buffer-local 'hs-fold-level)

(defvar whitespace-fold-level tab-width "whitespace folding level")
(make-variable-buffer-local 'whitespace-fold-level)

(defun hs-fold-increase ()
  (interactive)
  (setq hs-fold-level
        (1+ hs-fold-level))
  (hs-hide-level hs-fold-level))

(defun hs-fold-decrease ()
  (interactive)
  (setq hs-fold-level
        (max (1- hs-fold-level) 1))
  (hs-hide-level hs-fold-level))

(defun hs-fold-reset ()
  (interactive)
  (setq hs-fold-level 0)
  (fold-dwim-show-all))

(defun hs-fold-levels ()
  (interactive)

  (setq hs-fold-level 1)
  (hs-hide-level hs-fold-level)

  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<right>")	'hs-fold-increase)
     (define-key map (kbd "<left>") 	'hs-fold-decrease)
     (define-key map (kbd "SPC")    	'hs-fold-reset)
     map) t)
  (message "<right> to fold more, <left> to fold less, SPC to reset."))

(defun whitespace-fold-increase ()
  (interactive)
  (setq whitespace-fold-level
        (+ whitespace-fold-level 1))
  (set-selective-display whitespace-fold-level))

(defun whitespace-fold-decrease ()
  (interactive)
  (setq whitespace-fold-level
        (max (- whitespace-fold-level 1) 1))
  (set-selective-display whitespace-fold-level))

(defun whitespace-fold-reset ()
  (interactive)
  (setq whitespace-fold-level 0)
  (set-selective-display whitespace-fold-level))

(defun whitespace-fold-levels ()
  (interactive)

  (setq whitespace-fold-level 1)
  (set-selective-display whitespace-fold-level)

  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<right>")	'whitespace-fold-increase)
     (define-key map (kbd "<left>") 	'whitespace-fold-decrease)
     (define-key map (kbd "SPC")    	'whitespace-fold-reset)
     map) t)
  (message "<right> to fold more, <left> to fold less, SPC to reset."))

(provide 'setup-folding)
