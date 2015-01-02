(defvar hledger-mode-hook nil)

(defvar hledger-mode-map (make-keymap))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hledger\\.journal" . hledger-mode))

(defconst hledger-font-lock-keywords-1
  (list
   '("\\<\\(assets\\|liabilities\\|equity\\|expenses\\|income\\)\\>" . font-lock-variable-name-face)
   '("\\<[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\>" . font-lock-string-face)
   '("[0-9,]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   '("@" . font-lock-builtin-face)
   )
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-keywords hledger-font-lock-keywords-1
  "Default highlighting expressions for hledger mode")

(defun hledger-indent-line ()
  "Indent current line as hledger journal"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\>")
	  (progn
	    (setq cur-indent 0)
	    (setq not-indented nil)))
      (save-excursion
	(while not-indented
	  (forward-line -1)
	  (if (looking-at "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\>")
	      (progn
		(setq cur-indent default-tab-width)
		(setq not-indented nil))
	    (if (or (looking-at "^[ \t]*$") (bobp))
		(progn
		  (setq cur-indent 0)
		  (setq not-indented nil))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defvar hledger-mode-syntax-table (make-syntax-table))

(defun hledger-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table hledger-mode-syntax-table)
  (use-local-map hledger-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hledger-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'hledger-indent-line)
  (setq major-mode 'hledger-mode)
  (setq mode-name "hledger")
  (run-hooks 'hledger-mode-hook))

(provide 'hledger-mode)
