;; align
(use-package align
  :config
  ;; definitions for ruby code
  ;; fixes the most egregious mistake in detecting regions (hashes), but should be properly generalized at some point
  (setq align-region-separate "\\(^\\s-*[{}]?\\s-*$\\)\\|\\(=\\s-*[][{}()]\\s-*$\\)")
  (defconst align-ruby-modes '(enh-ruby-mode)
    "align-perl-modes is a variable defined in `align.el'.")
  (defconst ruby-align-rules-list
    '((ruby-comma-delimiter
       (regexp . ",\\(\\s-*\\)[^/ \t\n]")
       (modes . '(enh-ruby-mode))
       (repeat . t))
      (ruby-string-after-func
       (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
       (modes . '(enh-ruby-mode))
       (repeat . t))
      (ruby-symbol-after-func
       (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
       (modes . '(enh-ruby-mode))))
    "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")
  (add-to-list 'align-perl-modes         'enh-ruby-mode)
  (add-to-list 'align-dq-string-modes    'enh-ruby-mode)
  (add-to-list 'align-sq-string-modes    'enh-ruby-mode)
  (add-to-list 'align-open-comment-modes 'enh-ruby-mode)
  (dolist (it ruby-align-rules-list)
    (add-to-list 'align-rules-list it))
  ;; haskell alignments
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))

  (defun align-region-or-current ()
    "Align current selected region or implied region if nothing is selected."
    (interactive)
    (if (and mark-active
             (/= (point) (mark)))
        (align (point) (mark))
      (align-current)))

  ;; repeat regex (teh fuck ain't that the default?!)
  ;; (defun align-repeat (start end regexp)
  ;;   "Repeat alignment with respect to the given regular expression."
  ;;   (interactive "r\nsAlign regexp: ")
  ;;   (align-regexp start end
  ;;                 (concat "\\(\\s-*\\)" regexp) 1 1 t))
  (defun align-repeat (start end regexp &optional justify-right after)
    "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
    (interactive "r\nsAlign regexp: ")
    (let ((complete-regexp (if after
                               (concat regexp "\\([ \t]*\\)")
                             (concat "\\([ \t]*\\)" regexp)))
          (group (if justify-right -1 1)))
      (align-regexp start end complete-regexp group 1 t)))
  
  ;; Modified answer from
  ;; http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
  (defun align-repeat-decimal (start end)
    "Align a table of numbers on decimal points and dollar signs (both optional)."
    (interactive "r")
    (require 'align)
    (align-region start end nil
                  '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                         (repeat . t)
                         (group 1 2)
                         (spacing 1 1)
                         (justify nil t)))
                  nil))  
  
  (defmacro create-align-repeat-x (name regexp &optional justify-right default-after)
    (let ((new-func (intern (concat "align-repeat-" name))))
      `(defun ,new-func (start end switch)
         (interactive "r\nP")
         (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
           (align-repeat start end ,regexp ,justify-right after)))))
  
  (defun align-whitespace (start end)
    "Align region by whitespace."
    (interactive "r")
    (align-regexp start end (concat "\\(\\s-*\\)" "\\s-") 1 0 t))

  ;; align should always indent with spaces
  (defadvice align-areas (around fix-tab-indent activate)
    (let ((indent-tabs-mode nil))
      ad-do-it))
  
  (create-align-repeat-x "comma" "," nil t)
  (create-align-repeat-x "semicolon" ";" nil t)
  (create-align-repeat-x "colon" ":" nil t)
  (create-align-repeat-x "equal" "=")
  (create-align-repeat-x "math-oper" "[+\\-*/]")
  (create-align-repeat-x "ampersand" "&")
  (create-align-repeat-x "bar" "|")
  (create-align-repeat-x "left-paren" "(")
  (create-align-repeat-x "right-paren" ")" t)

  ;; align current region
  (bind-keys :prefix-map align-prefix-map
             :prefix "C-="
             ("a"   . align-region-or-current)
             ("SPC" . align-repeat)
             ("r"   . align-repeat)
             ("w"   . align-whitespace)
             ("m"   . align-repeat-math-oper)
             ("."   . align-repeat-decimal)
             (","   . align-repeat-comma)
             (";"   . align-repeat-semicolon)
             (":"   . align-repeat-colon)
             ("="   . align-repeat-equal)
             ("&"   . align-repeat-ampersand)
             ("|"   . align-repeat-bar)
             ("("   . align-repeat-left-paren)
             (")"   . align-repeat-right-paren)))

(provide 'setup-align)
