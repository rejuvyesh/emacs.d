(require 'autoinsert)
;;(auto-insert-mode)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates/")
(setq auto-insert 'other)
(setq auto-insert-query nil)
(setq auto-insert-alist '(("\\.sh$" . ["sh" rejuvyesh/auto-update-defaults])
                          ("\\.c$" . ["c" rejuvyesh/auto-update-c-source-file])
                          ("\\.h$" . ["h" rejuvyesh/auto-update-header-file])
                          ))



(defun rejuvyesh/auto-replace-header-name ()
  (save-excursion
    (while (search-forward "###" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (upcase (file-name-nondirectory buffer-file-name)))
        (subst-char-in-region (point-min) (point-max) ?. ?_)
        (subst-char-in-region (point-min) (point-max) ?- ?_)
        ))
    )
  )

(defun rejuvyesh/auto-replace-file-name ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name) t)
        ))
    )
  )

(defun rejuvyesh/auto-replace-file-name-no-ext ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE_NO_EXT<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-sans-extension (file-name-nondirectory buffer-file-name)) t)
        ))
    )
  )

(defun rejuvyesh/insert-today ()
  "Insert today's date into buffer"
  (interactive)
  (insert (format-time-string "%A, %B %e %Y" (current-time))))

(defun rejuvyesh/auto-replace-date-time ()
  (save-excursion
    ;; replace DDDD with today's date
    (while (search-forward "(>>DATE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match "" t)
        (rejuvyesh/insert-today)
        ))))

(defun rejuvyesh/auto-update-header-file ()
  (rejuvyesh/auto-replace-header-name)
  (rejuvyesh/auto-replace-file-name)
  )

(defun rejuvyesh/auto-update-c-source-file ()
  (save-excursion
    ;; Replace HHHH with file name sans suffix
    (while (search-forward "HHHH" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h") t))))
  (rejuvyesh/auto-replace-file-name)
  (rejuvyesh/auto-replace-date-time))

(defun rejuvyesh/auto-update-defaults ()
  (rejuvyesh/auto-replace-file-name)
  (rejuvyesh/auto-replace-file-name-no-ext)
  (rejuvyesh/auto-replace-date-time)
  )

;;; emacs-rc-auto-insert.el ends here

