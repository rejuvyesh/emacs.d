;;; notes-mode.el --- Emacs Major mode for thinking in text
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;; This mode implements the notation I use to write notes (and teaching scripts). It helps you write any documentation in the most desirable style, that of prompted examples, by annotating normal plain text. It works like this:
;;
;; [ line annotations
;;
;; You can mark a line as special by starting it with a dedicated character, like this:
;;
;; $ This line is an explanation. You can tell because it starts with a $.
;;
;; $ The next line is a quote. It starts with a >.
;; > I am a quote!
;; $ (Yes, you are.)
;;
;; % How do you start a quote?
;; @ With a >.
;;
;; $ As you can see, a % begins a prompt, whereas @ marks the expected reply or answer. They are typically used as a pair, but @ by itself is also used to show examples.
;;
;; Remaining options:
;;
;; + abstract concept, as demonstrated by the following lines
;; # comments
;; ! wrong, invalid
;; = equivalent, typically used to transform metaphors into literal structures
;;
;; ]
;;
;; [ highlighting
;;
;; emphasis:
;; @ I suspect stegosaurus was *really* dumb.
;;
;; placeholders:
;; @ I used to work for [big company] before I moved to Alaska.
;;
;; [ grab brackets
;;
;; Enclose text in any pair of parentheses. You can add headers too, like I'm been doing.
;;
;; ]
;; [ lists
;;
;; Works like Markdown, but only supports the "-" or "1." notation.
;;
;; ]
;; [ links
;;
;; Like Markdown.
;;
;; ]
;; [ footnotes
;;
;; Like extended Markdown, except also allows nesting.
;;
;; ]
;; [ indentation
;;
;; ]
;; [ folding
;;
;; ]
;; [ tables
;;
;; ]
;;
;; [ appendix
;;
;; The primary stylistic influences are:
;;
;; - Markdown (and the common extensions to it), including Jason R. Blevins
;;   <jrblevin@sdf.org>'s markdown-mode as a starting codebase.
;;
;; - Owen Richardson <owen.a.richardson@gmail.com>'s use of grab brackets and
;;   choice of line annotations. (<3)
;;
;; - Knuth's idea of literate programming, in a vague sense
;;
;; - the experience of writing language courses and needing a convenient notation for common constructions, like prompts
;; ]
;;
;;; Installation:
;;
;; Put the `notes-mode.el` somewhere in the load-path, typically something like `~/.emacs.d/notes-mode.el`.
;;
;; Add the following lines to your `.emacs` file to associate notes-mode
;; with `.txt` files:
;;
;;     (require 'notes-mode)
;;     (add-to-list 'auto-mode-alist '("\\.txt\\'" . notes-mode))

;;; Code:

(eval-when-compile (require 'cl))

;;; Customizable variables ====================================================

(defvar notes-mode-hook nil
  "Hook run when entering Markdown mode.")

(defgroup notes nil
  "Major mode for editing text files in Notes format."
  :prefix "notes-"
  :group 'wp)

(defcustom notes-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https"
    "imap" "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero"
    "rtsp" "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'notes
  :type 'list)

;;; Font lock =================================================================

(require 'font-lock)

(defvar notes-italic-face 'notes-italic-face
  "Face name to use for italic text.")

(defvar notes-bold-face 'notes-bold-face
  "Face name to use for bold text.")

(defvar notes-placeholder-face 'notes-placeholder-face
  "Face name to use for placeholder text.")

(defvar notes-annotation-prompt-face 'notes-annotation-prompt-face
  "Face name to use as a base for prompt annotation.")

(defvar notes-annotation-reply-face 'notes-annotation-reply-face
  "Face name to use as a base for reply annotation.")

(defvar notes-annotation-quote-face 'notes-annotation-quote-face
  "Face name to use as a base for quote annotation.")

(defvar notes-annotation-abstract-face 'notes-annotation-abstract-face
  "Face name to use as a base for abstract annotation.")

(defvar notes-annotation-comment-face 'notes-annotation-comment-face
  "Face name to use as a base for comment annotation.")

(defvar notes-annotation-model-face 'notes-annotation-model-face
  "Face name to use as a base for model annotation.")

(defvar notes-annotation-wrong-face 'notes-annotation-wrong-face
  "Face name to use as a base for wrong annotation.")

(defvar notes-annotation-equivalent-face 'notes-annotation-equivalent-face
  "Face name to use as a base for equivalent annotation.")

(defvar notes-annotation-code-face 'notes-annotation-code-face
  "Face name to use as a base for code annotation.")

(defvar notes-bracket-face 'notes-bracket-face
  "Face name to use as a base for grab brackets.")

(defvar notes-header-face 'notes-header-face
  "Face name to use as a base for headers.")

(defvar notes-list-face 'notes-list-face
  "Face name to use for list markers.")

(defvar notes-link-face 'notes-link-face
  "Face name to use for links.")

(defvar notes-reference-face 'notes-reference-face
  "Face name to use for references.")

(defvar notes-footnote-face 'notes-footnote-face
  "Face name to use for footnote identifiers.")

(defvar notes-url-face 'notes-url-face
  "Face name to use for URLs.")

(defvar notes-link-title-face 'notes-link-title-face
  "Face name to use for reference link titles.")

;; customization

(defgroup notes-faces nil
  "Faces used in Notes Mode"
  :group 'notes
  :group 'faces)

(defface notes-italic-face
  '((t (:slant italic)))
  "Face for italic text."
  :group 'notes-faces)

(defface notes-bold-face
  '((t (:weight bold)))
  "Face for bold text."
  :group 'notes-faces)

(defface notes-placeholder-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for placeholder text."
  :group 'notes-faces)

(defface notes-bracket-face
  '((t (:inherit font-lock-builtin-name-face :weight bold)))
  "Base face for grab brackets."
  :group 'notes-faces)

(defface notes-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'notes-faces)

(defface notes-annotation-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for code annotation."
  :group 'notes-faces)

(defface notes-annotation-quote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for quote annotation."
  :group 'notes-faces)

(defface notes-annotation-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for comment annotations."
  :group 'notes-faces)

(defface notes-annotation-prompt-face
  '((t (:inherit font-lock-doc-face)))
  "Face for prompt annotation."
  :group 'notes-faces)

(defface notes-annotation-reply-face
  '((t (:inherit font-lock-doc-face)))
  "Face for reply annotation."
  :group 'notes-faces)

(defface notes-annotation-abstract-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for abstract annotation."
  :group 'notes-faces)

(defface notes-annotation-model-face
  '((t (:inherit font-lock-doc-face)))
  "Face for model annotation."
  :group 'notes-faces)

(defface notes-annotation-wrong-face
  '((t (:inherit font-lock-warning-face)))
  "Face for wrong annotation."
  :group 'notes-faces)

(defface notes-annotation-equivalent-face
  '((t (:inherit font-lock-doc-face)))
  "Face for equivalent annotation."
  :group 'notes-faces)

(defface notes-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'notes-faces)

(defface notes-link-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'notes-faces)

(defface notes-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'notes-faces)

(defface notes-reference-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'notes-faces)

(defface notes-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs."
  :group 'notes-faces)

(defface notes-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'notes-faces)

;; parsing

(defconst notes-regex-link-inline
  "\\(!?\\[[^]]*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).")

(defconst notes-regex-link-reference
  "\\(!?\\[[^]]+?\\]\\)[ \t]?\\(\\[[^]]*?\\]\\)"
  "Regular expression for a reference link [text][id].")

(defconst notes-regex-reference-definition
  "^[ \t]*\\(\\[[^^]+?\\]\\):\\s *\\(.*?\\)[ \t]*\\( \"[^\"]*\"$\\|$\\)"
  "Regular expression for a link definition [id]: ...")

(defconst notes-regex-footnote
  "\\(\\[\\^.+?\\]\\)"
  "Regular expression for a footnote marker [^fn].")

(defconst notes-regex-header
  "^\\([ \t]*\\)\\([\\[{<][ \t]*\\)\\(.+\\)"
  "Regular expression for headers.")

(defconst notes-regex-annotation-abstract
  "^\\([ \t]*\\)\\([+][ \t]+\\)\\(.*\\)"
  "Regular expression for abstract annotation.")

(defconst notes-regex-annotation-code
  "^\\([ \t]*\\)\\([~][ \t]+\\)\\(.*\\)"
  "Regular expression for code annotation.")

(defconst notes-regex-annotation-comment
  "^\\([ \t]*\\)\\([#][ \t]+\\)\\(.*\\)"
  "Regular expression for comment annotation.")

(defconst notes-regex-annotation-equivalent
  "^\\([ \t]*\\)\\([=][ \t]+\\)\\(.*\\)"
  "Regular expression for equivalent annotation.")

(defconst notes-regex-annotation-model
  "^\\([ \t]*\\)\\([$][ \t]+\\)\\(.*\\)"
  "Regular expression for model annotation.")

(defconst notes-regex-annotation-prompt
  "^\\([ \t]*\\)\\([%][ \t]+\\)\\(.*\\)"
  "Regular expression for prompt annotation.")

(defconst notes-regex-annotation-quote
  "^\\([ \t]*\\)\\([>][ \t]+\\)\\(.*\\)"
  "Regular expression for quote annotation.")

(defconst notes-regex-annotation-reply
  "^\\([ \t]*\\)\\([@][ \t]+\\)\\(.*\\)"
  "Regular expression for reply annotation.")

(defconst notes-regex-annotation-wrong
  "^\\([ \t]*\\)\\([!][ \t]+\\)\\(.*\\)"
  "Regular expression for  annotation.")

(defconst notes-regex-list
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[-]\\)\\([ \t]+\\)"
  "Regular expression for matching list items.")

(defconst notes-regex-bold
  "\\(^\\|[ \t]\\)\\([*]\\(.+?\\)[*]\\)"
  "Regular expression for matching bold text.")

(defconst notes-regex-italic
  "\\(^\\|[ \t]\\)\\([/]\\(.+?\\)[/]\\)"
  "Regular expression for matching italic text.")

(defconst notes-regex-placeholder
  "\\(\\[.+?\\]\\)[^[]?"
  "Regular expression for matching placeholder text.")

(defconst notes-regex-uri
  (concat
   "\\(" (mapconcat 'identity notes-uri-types "\\|")
   "\\):[^]\t\n\r<>,;() ]+")
  "Regular expression for matching inline URIs.")

(defconst notes-regex-angle-uri
  (concat
   "\\(<\\)\\(\\(?:"
   (mapconcat 'identity notes-uri-types "\\|")
   "\\):[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst notes-regex-email
  "<\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+>"
  "Regular expression for matching inline email addresses.")

(defconst notes-regex-list-indent
  "^\\([ \t]*\\)\\([0-9]+\\.\\|[-]\\)\\([ \t]+\\)"
  "Regular expression for matching indentation of list items.")

;; keywords

(defvar notes-mode-font-lock-keywords
  (list
   ;; (cons 'notes-match-fenced-code-blocks '((0 notes-pre-face)))
   ;; (cons notes-regex-blockquote 'notes-blockquote-face)
   ;; (cons 'notes-match-comments '((0 notes-comment-face t t)))
   ;; (cons notes-regex-code '(2 notes-inline-code-face))
   (cons notes-regex-angle-uri             'notes-link-face)
   (cons notes-regex-uri                   'notes-link-face)
   (cons notes-regex-email                 'notes-link-face)
   (cons notes-regex-list                  '(2 notes-list-face))
   (cons notes-regex-link-inline           '((1 notes-link-face t)
                                             (2 notes-url-face t)))
   (cons notes-regex-link-reference        '((1 notes-link-face t)
                                             (2 notes-reference-face t)))
   (cons notes-regex-reference-definition  '((1 notes-reference-face t)
                                             (2 notes-url-face t)
                                             (3 notes-link-title-face t)))
   (cons notes-regex-footnote              'notes-footnote-face)

   (cons notes-regex-bold                  '(2 notes-bold-face))
   (cons notes-regex-italic                '(2 notes-italic-face))
   (cons notes-regex-placeholder           '(1 notes-placeholder-face))

   (cons notes-regex-annotation-abstract   '(2 notes-annotation-abstract-face))
   (cons notes-regex-annotation-code       '(2 notes-annotation-code-face))
   (cons notes-regex-annotation-comment    '(2 notes-annotation-comment-face))
   (cons notes-regex-annotation-equivalent '(2 notes-annotation-equivalent-face))
   (cons notes-regex-annotation-model      '(2 notes-annotation-model-face))
   (cons notes-regex-annotation-prompt     '(2 notes-annotation-prompt-face))
   (cons notes-regex-annotation-quote      '(2 notes-annotation-quote-face))
   (cons notes-regex-annotation-reply      '(2 notes-annotation-reply-face))
   (cons notes-regex-annotation-wrong      '(2 notes-annotation-wrong-face))

   (cons notes-regex-header                '(3 notes-header-face))
)
  "Syntax highlighting for Notes files.")

;;; Notes parsing functions ================================================

(defun notes-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "^\\s *$" (point-at-eol) t)))

(defun notes-prev-line-blank-p ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-min))
        t
      (forward-line -1)
      (notes-cur-line-blank-p))))

(defun notes-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (save-excursion
    (if (= (point-at-bol) (point-max))
        t
      (forward-line 1)
      (notes-cur-line-blank-p))))

(defun notes-prev-line-indent-p ()
  "Return t if the previous line is indented and nil otherwise."
  (save-excursion
    (forward-line -1)
    (goto-char (point-at-bol))
    (if (re-search-forward "^\\s " (point-at-eol) t) t)))

(defun notes-cur-line-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (re-search-forward "^\\s +" (point-at-eol) t)
    (current-column)))

(defun notes-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line."
  (save-excursion
    (forward-line -1)
    (when (notes-cur-line-blank-p)
      (forward-line -1))
    (notes-cur-line-indent)))

(defun notes-next-line-indent ()
  "Return the number of leading whitespace characters in the next line."
  (save-excursion
    (forward-line 1)
    (notes-cur-line-indent)))

(defun notes-cur-non-list-indent ()
  "Return the number of leading whitespace characters in the current line."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward notes-regex-list (point-at-eol) t)
      (current-column))))

(defun notes-prev-non-list-indent ()
  "Return position of the first non-list-marker on the previous line.
If the previous line is empty, check the line before that one, too."
  (save-excursion
    (forward-line -1)
    (when (notes-cur-line-blank-p)
       (forward-line -1))
    (notes-cur-non-list-indent)))

(defun notes--next-block ()
  "Move the point to the start of the next text block."
  (forward-line)
  (while (and (or (not (notes-prev-line-blank-p))
                  (notes-cur-line-blank-p))
              (not (eobp)))
    (forward-line)))

(defun notes--end-of-level (level)
  "Move the point to the end of region with indentation at least LEVEL."
  (let (indent)
    (while (and (not (< (setq indent (notes-cur-line-indent)) level))
                (not (>= indent (+ level 4)))
                (not (eobp)))
      (notes--next-block))
    (unless (eobp)
      ;; Move back before any trailing blank lines
      (while (and (notes-prev-line-blank-p)
                  (not (bobp)))
        (forward-line -1))
      (forward-line -1)
      (end-of-line))))

(defun notes-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (notes-cur-line-indent))
    (while
        (cond
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((notes-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at notes-regex-list)
               (setq bounds (notes-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (notes-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at notes-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at notes-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (notes-cur-line-indent)))
    prev))

(defun notes-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent next)
    (setq next (point))
    (forward-line)
    (setq indent (notes-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) (setq prev nil))
         ;; Continue if the current line is blank
         ((notes-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at notes-regex-list)
               (setq bounds (notes-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (notes-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at notes-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at notes-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (notes-cur-line-indent)))
    next))

(defun notes-cur-list-item-end (level)
  "Move to the end of the current list item with nonlist indentation LEVEL.
If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (notes-cur-line-indent))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((notes-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (notes-prev-line-blank-p))
          nil)
         ;; Stop at a new list item of the same or lesser indentation
         ((looking-at notes-regex-list) nil)
         ;; Stop at a header
         ((looking-at notes-regex-header) nil)
         ;; Stop at a horizontal rule
         ((looking-at notes-regex-hr) nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (notes-cur-line-indent)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (looking-back (concat notes-regex-list "\\s-*"))
          (goto-char (match-end 3))
      (skip-syntax-backward "-"))))

(defun notes-cur-list-item-bounds ()
  "Return bounds and indentation of the current list item.
Return a list of the form (begin end indent nonlist-indent).
If the point is not inside a list item, return nil.
Leave match data intact for `notes-regex-list'."
  (let (cur prev-begin prev-end indent nonlist-indent)
    ;; Store current location
    (setq cur (point))
    ;; Verify that cur is between beginning and end of item
    (save-excursion
      (if (looking-at notes-regex-list)
          (beginning-of-line)
        (end-of-line)
        (re-search-backward notes-regex-list nil t))
      (save-match-data
        (setq prev-begin (point))
        (setq indent (notes-cur-line-indent))
        (setq nonlist-indent (notes-cur-non-list-indent))
        (notes-cur-list-item-end nonlist-indent)
        (setq prev-end (point)))
      (if (and (>= cur prev-begin)
               (<= cur prev-end)
               nonlist-indent)
          (list prev-begin prev-end indent nonlist-indent)
        nil))))

;; From html-helper-mode
(defun notes-match-comments (last)
  "Match HTML comments from the point to LAST."
  (cond ((search-forward "<!--" last t)
         (backward-char 4)
         (let ((beg (point)))
           (cond ((search-forward-regexp "--[ \t]*>" last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun notes-match-pre-blocks (last)
  "Match Markdown pre blocks from point to LAST.
A region matches as if it is indented at least four spaces
relative to the nearest previous block of lesser non-list-marker
indentation."

  (let (cur-begin cur-end cur-indent prev-indent prev-list stop match found)
    ;; Don't start in the middle of a block
    (unless (and (bolp)
                 (notes-prev-line-blank-p)
                 (not (notes-cur-line-blank-p)))
      (notes--next-block))

    ;; Move to the first full block in the region with indent 4 or more
    (while (and (not (>= (setq cur-indent (notes-cur-line-indent)) 4))
                (not (>= (point) last)))
      (notes--next-block))
    (setq cur-begin (point))
    (notes--end-of-level cur-indent)
    (setq cur-end (point))
    (setq match nil)
    (setq stop (> cur-begin cur-end))

    (while (and (<= cur-end last) (not stop) (not match))
      ;; Move to the nearest preceding block of lesser (non-marker) indentation
      (setq prev-indent (+ cur-indent 1))
      (goto-char cur-begin)
      (setq found nil)
      (while (and (>= prev-indent cur-indent)
                  (not (and prev-list
                            (eq prev-indent cur-indent)))
                  (not (bobp)))

        ;; Move point to the last line of the previous block.
        (forward-line -1)
        (while (and (notes-cur-line-blank-p)
                    (not (bobp)))
          (forward-line -1))

        ;; Update the indentation level using either the
        ;; non-list-marker indentation, if the previous line is the
        ;; start of a list, or the actual indentation.
        (setq prev-list (notes-cur-non-list-indent))
        (setq prev-indent (or prev-list
                              (notes-cur-line-indent)))
        (setq found t))

      ;; If the loop didn't execute
      (unless found
        (setq prev-indent 0))

      ;; Compare with prev-indent minus its remainder mod 4
      (setq prev-indent (- prev-indent (mod prev-indent 4)))

      ;; Set match data and return t if we have a match
      (if (>= cur-indent (+ prev-indent 4))
          ;; Match
          (progn
            (setq match t)
            (set-match-data (list cur-begin cur-end))
            ;; Leave point at end of block
            (goto-char cur-end)
            (forward-line))

        ;; Move to the next block (if possible)
        (goto-char cur-end)
        (notes--next-block)
        (setq cur-begin (point))
        (setq cur-indent (notes-cur-line-indent))
        (notes--end-of-level cur-indent)
        (setq cur-end (point))
        (setq stop (equal cur-begin cur-end))))
    match))

(defun notes-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (cond ((search-forward-regexp "^\\([~]\\{3,\\}\\)" last t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line)
           (cond ((search-forward-regexp
                   (concat "^" (match-string 1) "~*") last t)
                  (set-match-data (list beg (point)))
                  t)
                 (t nil))))
        (t nil)))

(defun notes-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))

;;; Syntax Table ==============================================================

(defvar notes-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `notes-mode'.")

;;; Indentation ====================================================================

(defun notes-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun notes-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `notes-enter-key' or
`notes-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `notes-enter-key', by an initial call of
`notes-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position."
  (interactive)
  (let ((positions (notes-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'notes-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (notes-indent-find-next-position cur-pos positions)))))

(defun notes-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level."
  (let (pos prev-line-pos positions)

    ;; Previous line indent
    (setq prev-line-pos (notes-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Previous non-list-marker indent
    (setq pos (notes-prev-non-list-indent))
    (when pos
      (setq positions (cons pos positions))
      (setq positions (cons (+ pos tab-width) positions)))

    ;; Indentation of the previous line + tab-width
    (cond
     (prev-line-pos
      (setq positions (cons (+ prev-line-pos tab-width) positions)))
     (t
      (setq positions (cons tab-width positions))))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos
             (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of preceeding list item
    (setq pos
          (save-excursion
            (forward-line -1)
            (catch 'break
              (while (not (equal (point) (point-min)))
                (forward-line -1)
                (goto-char (point-at-bol))
                (when (re-search-forward notes-regex-list (point-at-eol) t)
                  (throw 'break (length (match-string 1)))))
              nil)))
    (if (and pos (not (eq pos prev-line-pos)))
        (setq positions (cons pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    (reverse positions)))

(defun notes-dedent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then dedent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (let ((cur-pos (current-column))
        (start-of-indention (save-excursion
                              (back-to-indentation)
                              (current-column))))
    (if (and (> cur-pos 0) (= cur-pos start-of-indention))
        (let ((result 0))
          (dolist (i (notes-calc-indents))
            (when (< i cur-pos)
              (setq result (max result i))))
          (indent-line-to result))
      (backward-delete-char-untabify arg))))

;;; Keymap ====================================================================

(defvar notes-mode-map
  (let ((map (make-keymap)))
    ;; Indentation
    (define-key map (kbd "<backspace>") 'notes-dedent-or-delete)
    ;; Visibility cycling
    (define-key map "\C-i" 'notes-cycle)
    (define-key map "\M-i" 'notes-shifttab)
    ;; Lists
    ;; (define-key map "\C-c\C-cn" 'notes-cleanup-list-numbers)
    (define-key map (kbd "S-<return>") 'notes-insert-list-item)
    ;; (define-key map (kbd "C-<return>") 'notes-insert-list-item)
    map)
  "Keymap for Markdown major mode.")

;;; Lists =====================================================================

(defun notes-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is 4),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is 16),
increase the indentation by one level."
  (interactive "p")
  (let (bounds item-indent marker indent new-indent end)
    (save-match-data
      (setq bounds (notes-cur-list-item-bounds))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (notes-cur-line-blank-p)
              (insert "\n"))
            (insert "* "))
        ;; Compute indentation for a new list item
        (setq item-indent (nth 2 bounds))
        (setq marker (concat (match-string 2) (match-string 3)))
        (setq indent (cond
                      ((= arg 4) (max (- item-indent 4) 0))
                      ((= arg 16) (+ item-indent 4))
                      (t item-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char (nth 1 bounds))
        (newline)
        (cond
         ;; Ordered list
         ((string-match "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = item-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (not (looking-at (concat new-indent "\\([0-9]+\\)\\.")))
                (forward-line -1)))
            (insert (concat new-indent
                            (int-to-string (1+ (string-to-number (match-string 1))))
                            ". "))))
         ;; Unordered list
         ((string-match "[-]" marker)
          (insert (concat new-indent marker))))))))


;; ;;; Miscellaneous =============================================================

(defun notes-nobreak-p ()
  "Return nil if it is acceptable to break the current line at the point."
  ;; inside in square brackets (e.g., link anchor text)
  (looking-back "\\[[^]]*"))

;;; Mode definition  ==========================================================

;;;###autoload
(define-derived-mode notes-mode text-mode "Notes"
  "Major mode for editing Markdown files."
  ;; Ruby/C-style tab width
  (setq tab-width 2)

  ;; ;; comments
  ;; (make-local-variable 'comment-start)
  ;; (setq comment-start "<!-- ")
  ;; (make-local-variable 'comment-end)
  ;; (setq comment-end " -->")
  ;; (make-local-variable 'comment-start-skip)
  ;; (setq comment-start-skip "<!--[ \t]*")
  ;; (make-local-variable 'comment-column)
  ;; (setq comment-column 0)

  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(notes-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'font-lock-extend-region-functions
            'notes-font-lock-extend-region)

  ;; Make filling work with lists (unordered, ordered, and definition) and quotes.
  (set (make-local-variable 'paragraph-start)
       "\f\\|[ \t]*$\\|^[ \t]*[*+-] \\|^[ \t]*[0-9]+\\.\\|^[ \t]*: \\|^[ \t]*> ")

  ;; Indentation and filling
  (make-local-variable 'fill-nobreak-predicate)
  (add-hook 'fill-nobreak-predicate 'notes-nobreak-p)
  )

(provide 'notes-mode)

;;; notes-mode.el ends here
