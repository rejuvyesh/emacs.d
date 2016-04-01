;; for functions
(defun julia-split-args (arg-string)
  "Split a julia argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*[,;][[:blank:]]*" t)))

(defun julia-args-to-docstring ()
  "return docstring format for the julia arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (julia-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat "- " (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " : "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "# Arguments" formatted-args) indent))))


;; for types (not working yet)

;; (defun julia-split-type (code-string)
;;   "Split a julia type definition string"
;;   (split-string code-string "[[:blank:]]*\n[[:blank:]]*" t))

;; (defun julia-type-to-docstring ()
;;   "return docstring format for the type fields for yas-text"
;;   (let* ((indent (concat "\n" (make-string (current-column) 32)))
;;          (args (julia-split-type yas-text))
;;          (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
;;          (formatted-args (mapconcat
;;                 (lambda (x)
;;                    (concat "- " (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " : "
;;                            (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
;;                 args
;;                 indent)))

;;     (unless (string= formatted-args "")
;;       (mapconcat 'identity (list "# Fields" formatted-args) indent))))
