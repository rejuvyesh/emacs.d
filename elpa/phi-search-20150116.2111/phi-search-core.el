;;; phi-search-core.el --- another incremental search & replace, compatible with "multiple-cursors"

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 2.1.1

;;; Commentary:

;; This library is required by phi-search, phi-replace, etc.

;;; Change Log:

;; 1.0.0 divided from phi-search.el 1.2.2
;; 1.1.0 handle "isearch-open-invisible" properties
;; 1.2.0 implement "guess" option for "phi-search-case-sensitive"
;; 1.2.1 add variable "convert-query-function"
;; 1.2.2 add customizable variable "phi-search-hook"
;; 1.2.3 bug fix
;; 1.3.0 add highlight to mismatch part of search string
;; 1.3.1 add support for subword/jaword-mode
;; 2.0.0 use minibuffer to read query
;; 2.1.0 use "phi-search--message" to display messages
;; 2.1.1 add option phi-search-highlight-mismatch-part

;;; Code:

(defconst phi-search-core-version "2.1.1")

;; + customs

(defgroup phi-search nil
  "another incremental search interface"
  :group 'emacs)

(defcustom phi-search-limit 1000
  "maximum number of accepted matches"
  :group 'phi-search)

(defcustom phi-search-case-sensitive nil
  "when non-nil, phi-search become case sensitive"
  :group 'phi-search)

(defcustom phi-search-highlight-mismatch-part t
  "when non-nil, mismatch part of the input is highlighted."
  :group 'phi-search)

(defcustom phi-search-default-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "C-s") 'phi-search-again-or-next)
    (define-key kmap (kbd "C-r") 'phi-search-again-or-previous)
    (define-key kmap [remap phi-search] 'phi-search-again-or-next)
    (define-key kmap [remap phi-search-backward] 'phi-search-again-or-previous)
    (define-key kmap [remap keyboard-quit] 'phi-search-abort)
    (define-key kmap [remap scroll-up] 'phi-search-scroll-up)
    (define-key kmap [remap pager-page-down] 'phi-search-scroll-up)
    (define-key kmap [remap scroll-up-command] 'phi-search-scroll-up)
    (define-key kmap [remap pager-page-up] 'phi-search-scroll-down)
    (define-key kmap [remap scroll-down-command] 'phi-search-scroll-down)
    (define-key kmap [remap recenter] 'phi-search-recenter)
    (define-key kmap [remap kill-region] 'phi-search-yank-word)
    (define-key kmap [remap phi-rectangle-kill-region] 'phi-search-yank-word)
    (define-key kmap (kbd "RET") 'phi-search-complete)
    (define-key kmap (kbd "C-c C-c") 'phi-search-unlimit)
    kmap)
  "keymap for the phi-search prompt buffers"
  :group 'phi-search)

(defcustom phi-search-hook nil
  "hook run when phi-search buffer is prepared."
  :group 'phi-search)

;; + faces

(defface phi-search-match-face
  '((((background light)) (:background "#b5dee9"))
    (t (:background "#194854")))
  "Face used to highlight matching items in phi-search."
  :group 'phi-search)

(defface phi-search-selection-face
  '((((background light)) (:background "#e0d9de"))
    (t (:background "#594854")))
  "Face used to highlight selected items in phi-search."
  :group 'phi-search)

(defface phi-search-failpart-face
  '((t (:inherit 'isearch-fail)))
  "Face used to highlight mismatch part in phi-search buffer."
  :group 'phi-search)

;; + utilities

(defun phi-search--search-forward (query limit &optional filter inclusive)
  "a handy version of search-forward-regexp, that phi-search uses
to search for candidates. like (search-forward-regexp <> nil t)
but case-sensitivity is handled automatically and result is
filtered with FILTER. zero-width match is accepted only when
INCLUSIVE is non-nil."
  (let* ((case-fold-search (or (not phi-search-case-sensitive)
                               (and (eq phi-search-case-sensitive 'guess)
                                    (string= query (downcase query)))))
         (pos1 (point))
         (pos2 (search-forward-regexp query limit t)))
    (if (or (and (not inclusive) pos2 (= pos1 pos2))
            (and filter (not (save-match-data (funcall filter)))))
        (progn
          (forward-char 1)
          (phi-search--search-forward query limit filter t))
      pos2)))

(defun phi-search--open-invisible-temporary (hidep)
  "show invisible text at point temporary. when optional arg
HIDEP is non-nil, hide the opened text instead."
  (mapc (lambda (ov)
          (let ((ioit (overlay-get ov 'isearch-open-invisible-temporary)))
            (cond (ioit
                   (funcall ioit ov hidep))
                  ((overlay-get ov 'isearch-open-invisible)
                   (if hidep
                       (overlay-put ov 'invisible (overlay-get ov 'phi-invisible))
                     (overlay-put ov 'phi-invisible (overlay-get ov 'invisible))
                     (overlay-put ov 'invisible nil))))))
        (overlays-at (point))))

(defun phi-search--open-invisible-permanently ()
  "show invisible text at point permanently"
  (mapc (lambda (ov)
          (let ((ioi (overlay-get ov 'isearch-open-invisible)))
            (when ioi (funcall ioi ov))))
        (overlays-at (point))))

(defun phi-search--valid-regex-p (regex)
  "non-nil if REGEX is a valid regular expression"
  (ignore-errors
    (string-match regex "")
    t))

(declare-function sublimity--pre-command "sublimity")
(declare-function sublimity--post-command "sublimity")
(defmacro phi-search--with-sublimity (&rest body)
  "if sublimity is installed, use it"
  `(cond ((and (boundp 'sublimity-mode) sublimity-mode)
          (sublimity--pre-command)
          (prog1 (progn ,@body)
            (sublimity--post-command)))
         (t
          ,@body)))

;; + private functions/variables for TARGET buffer
;; ++ variables

(defvar phi-search--last-executed nil
  "the last query used")
(make-variable-buffer-local 'phi-search--last-executed)

(defvar phi-search--filter-function nil
  "when non-nil, candidates are filtered with this function.")
(make-variable-buffer-local 'phi-search--filter-function)

(defvar phi-search--original-position nil
  "position where this search is started from.")
(make-variable-buffer-local 'phi-search--original-position)

(defvar phi-search--overlays nil
  "an ordered list of active overlays in this target buffer.")
(make-variable-buffer-local 'phi-search--overlays)

(defvar phi-search--failed nil
  "non-nil if the last search was failed. 'err especially when
  the failure is caused by an error. `phi-search--overlays' can
  be nil on both failure and too-many-matches error, but this
  variable become non-nil only on failure.")
(make-variable-buffer-local 'phi-search--failed)

(defvar phi-search--selection nil
  "index of currently selected item. if nothing's selected, this
  variable must be set nil.")
(make-variable-buffer-local 'phi-search--selection)

(defvar phi-search--after-update-function nil
  "function called IN THE TARGET BUFFER as soon as overlays are updated")
(make-variable-buffer-local 'phi-search--after-update-function)

(defvar phi-search--saved-modeline-format nil
  "saved modeline-format of the target buffer.")
(make-variable-buffer-local 'phi-search--saved-modeline-format)

;; ++ functions

(defun phi-search--delete-overlays (&optional keep-point)
  "delete all overlays in THIS target buffer, and go back to the
original position. when optional arg KEEP-POINT is non-nil,
delete overlays without movind the cursor."
  (mapc 'delete-overlay phi-search--overlays)
  (setq phi-search--overlays nil
        phi-search--selection nil)
  (unless keep-point
    (phi-search--open-invisible-temporary t)
    (goto-char phi-search--original-position)))

(defun phi-search--make-overlays-for (query &optional unlimited)
  "perform search with QUERY, and make overlays for all matching
items in THIS target buffer. when optional arg UNLIMITED is
omitted or nil, number of matches is limited to
`phi-search-limit'."
  (cond
   ((not (phi-search--valid-regex-p query))
    (setq phi-search--failed 'err)
    (phi-search--message "invalid input"))
   (t
    (save-excursion
      (let ((before nil) (after nil) (cnt 0))
        (goto-char (point-min))
        (while (and (phi-search--search-forward query nil phi-search--filter-function)
                    (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                      (overlay-put ov 'face 'phi-search-match-face)
                      (push ov (if (< (match-beginning 0) phi-search--original-position)
                                   before
                                 after))
                      (setq cnt (1+ cnt))
                      (or unlimited (< cnt phi-search-limit)))))
        (setq phi-search--overlays (nconc (nreverse after) (nreverse before)))))
    (let ((num (length phi-search--overlays)))
      (cond ((zerop num)
             (setq phi-search--failed t))
            (t
             (setq phi-search--failed nil)
             (when (and (not unlimited)
                        (>= num phi-search-limit))
               (phi-search--message "too short")
               (phi-search--delete-overlays))))))))

(defun phi-search--select (n)
  "select Nth item and move cursor there. return point on
success, or nil on failuare."
  (when (and (>= n 0)
             (< n (length phi-search--overlays)))
    ;; unselect old item
    (when phi-search--selection
      (phi-search--open-invisible-temporary t)
      (overlay-put (nth phi-search--selection phi-search--overlays)
                   'face 'phi-search-match-face))
    ;; select new item if there
    (let ((ov (nth n phi-search--overlays)))
      (setq phi-search--selection n)
      (overlay-put ov 'face 'phi-search-selection-face)
      (goto-char (overlay-end ov))
      (phi-search--open-invisible-temporary nil)
      (point))))

;; + private functions/variables for PROMPT buffer
;; ++ variables

;; *TODO* these variable may not have to be buffer-local. porting
;; local variables with `phi-search--with-target-buffer' was a poor
;; design. we probably should implement something like
;; `phi-search-get-query' function to fetch converted query.

(defvar phi-search--target nil
  "the target (window . buffer) which this prompt buffer is for")
(make-variable-buffer-local 'phi-search--target)

(defvar phi-search--before-complete-function nil
  "function called IN THIS PROMPT BUFFER just before phi-search
  completes")
(make-variable-buffer-local 'phi-search--before-complete-function)

(defvar phi-search--convert-query-function nil
  "function that converts search query.")
(make-variable-buffer-local 'phi-search--convert-query-function)

(defvar phi-search--fail-pos nil
  "save position where search fail begin with.")
(make-variable-buffer-local 'phi-search--fail-pos)

(defvar phi-search--message-start nil
  "starting position of a message. nil if no message is active.")
(make-variable-buffer-local 'phi-search--message-start)

(defvar phi-search--pending-message nil
  "a pending message string, or nil.")
(make-variable-buffer-local 'phi-search--pending-message)

;; ++ functions

(defvar phi-search--last-converted-query nil)
(defun phi-search--generate-query (q)
  (if (null phi-search--convert-query-function) q
    (funcall phi-search--convert-query-function q)))

(defun phi-search--message (msg)
  (with-selected-window (minibuffer-window)
    (setq phi-search--pending-message msg)))

(defmacro phi-search--with-target-buffer (&rest body)
  "eval body with the target buffer selected.
\"target\" and \"query\" are brought from the prompt buffer"
  `(progn
     ;; assert that the window and the buffer live
     (cond ((null phi-search--target)
            (error "phi-search: unexpected error (phi-search--target is nil)"))
           ((not (window-live-p (car phi-search--target)))
            (error "phi-search: target window is deleted"))
           ((not (buffer-live-p (cdr phi-search--target)))
            (error "phi-search: target buffer is killed")))
     ;; visit the window, with variables from the prompt buffer
     (let ((target phi-search--target)
           (query (phi-search--generate-query (minibuffer-contents))))
       (with-selected-window (car target)
         ;; if buffer is switched, switch back to the target
         (unless (eq (current-buffer) (cdr target))
           (switch-to-buffer (cdr target)))
         ;; eval body
         ,@body))))

(defun phi-search-next ()
  "select next item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "no matches"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1+ phi-search--selection))
      (phi-search--select 0)
      (ding)
      (message "no more matches")))))

(defun phi-search-previous ()
  "select previous item."
  (phi-search--with-target-buffer
   (when (null phi-search--selection)
     (error "no matches"))
   (phi-search--with-sublimity
    (unless (phi-search--select (1- phi-search--selection))
      (phi-search--select (1- (length phi-search--overlays)))
      (ding)
      (message "no more matches")))))

(defun phi-search--update (&optional pos &rest _)
  "update overlays for the target buffer. POS specifies the
position from where input is highlighted when search failed."
  (let ((status (phi-search--with-target-buffer
                 (phi-search--with-sublimity
                  (phi-search--delete-overlays)
                  (phi-search--make-overlays-for query)
                  (phi-search--select 0))
                 (when phi-search--after-update-function
                   (funcall phi-search--after-update-function))
                 phi-search--failed)))
    (when phi-search-highlight-mismatch-part
      (cond
       ((null status)                    ; success
        (setq phi-search--fail-pos nil)
        (put-text-property (minibuffer-prompt-end) (point-max) 'face nil))
       (t                                ; failure
        (setq phi-search--fail-pos (if (eq status 'err)
                                       (minibuffer-prompt-end)
                                     (or pos (point))))
        (put-text-property
         phi-search--fail-pos (or phi-search--message-start (point-max))
         'face 'phi-search-failpart-face))))))

(defun phi-search--clear-message (&rest _)
  "delete message part from minibuffer."
  (when phi-search--message-start
    (let ((inhibit-modification-hooks t))
      (delete-region phi-search--message-start (point-max)))
    (setq phi-search--message-start nil)))

(defun phi-search--restore-message ()
  "insert pending message to minibuffer"
  (when phi-search--pending-message
    (save-excursion
      (setq phi-search--message-start (goto-char (point-max)))
      (let ((inhibit-modification-hooks t))
        (insert " [" phi-search--pending-message "]")
        (setq phi-search--pending-message nil)))))

;; + select commands

(defun phi-search-again-or-next ()
  "search again with the last query, or search next item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (minibuffer-contents) ""))
        (phi-search-next)
      (when str (insert str)))))

(defun phi-search-again-or-previous ()
  "search again with the last query, or search previous item"
  (interactive)
  (let ((str (phi-search--with-target-buffer
              phi-search--last-executed)))
    (if (not (string= (minibuffer-contents) ""))
        (phi-search-previous)
      (when str (insert str)))))

;; + replace scroll commands

(defun phi-search-recenter ()
  "recenter target buffer"
  (interactive)
  (phi-search--with-target-buffer
   (when phi-search--selection
     (phi-search--with-sublimity
      (phi-search--select phi-search--selection)
      (recenter)))))

(defun phi-search-scroll-down ()
  "scroll down the target buffer"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (call-interactively 'scroll-down))))

(defun phi-search-scroll-up ()
  "scroll up the target buffer"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (call-interactively 'scroll-up))))

;; + other commands

(defun phi-search-unlimit ()
  "search for all occurrences, regardless of phi-search-limit"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)
    (phi-search--make-overlays-for query t)
    (phi-search--select 0)
    (when phi-search--after-update-function
      (funcall phi-search--after-update-function)))))

(declare-function subword-forward "subword" (&optional arg))
(declare-function jaword-forward "jaword" (&optional arg))
(defun phi-search-yank-word ()
  "If there's a region in query buffer, kill-region as usual.
Otherwise yank a word from target buffer and expand query."
  (interactive)
  (if (or (not (use-region-p))
          (= (region-beginning) (region-end)))
      (insert
       (phi-search--with-target-buffer
        (buffer-substring-no-properties
         (point)
         (save-excursion
           (cond ((and (boundp 'jaword-mode) jaword-mode) (jaword-forward 1))
                 ((and (boundp 'subword-mode) subword-mode) (subword-forward 1))
                 (t (forward-word)))
           (point)))))
    (kill-region (region-beginning) (region-end))))

;; + start/end phi-search

(defun phi-search--initialize (modeline-fmt keybinds filter-fn update-fn
                                            complete-fn &optional conv-fn init-fn prompt)
  (setq phi-search--saved-modeline-format  mode-line-format)
  (setq mode-line-format                   modeline-fmt
        phi-search--original-position      (point)
        phi-search--filter-function        filter-fn
        phi-search--after-update-function  update-fn
        phi-search--selection              nil
        phi-search--overlays               nil)
  (let ((wnd (selected-window))
        (buf (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; *FIXME* does wrong when a timer modifies the minibuffer
          ;; ('cause message is not cleared yet)
          (add-hook 'pre-command-hook 'phi-search--clear-message nil t)
          (add-hook 'after-change-functions 'phi-search--update nil t)
          (add-hook 'post-command-hook 'phi-search--restore-message nil t)
          (setq phi-search--target                   (cons wnd buf)
                phi-search--convert-query-function   conv-fn
                phi-search--before-complete-function complete-fn)
          (run-hooks 'phi-search-hook)
          (funcall init-fn))
      (read-from-minibuffer
       (or prompt "phi-search: ") nil
       (let ((kmap (copy-keymap phi-search-default-map)))
         (dolist (bind (reverse keybinds))
           (eval `(define-key kmap ,(car bind) ,(cdr bind))))
         kmap)))))

(defun phi-search-complete (&rest args)
  "finish phi-search. (for developers: ARGS are passed to complete-function)"
  (interactive)
  (when phi-search--before-complete-function
    (apply phi-search--before-complete-function args))
  (let ((wnd (car phi-search--target))
        (str (minibuffer-contents)))
    (phi-search--with-target-buffer
     (phi-search--delete-overlays t)
     (phi-search--open-invisible-permanently)
     (setq mode-line-format                   phi-search--saved-modeline-format
           phi-search--original-position      nil
           phi-search--filter-function        nil
           phi-search--after-update-function  nil
           phi-search--selection              nil
           phi-search--overlays               nil
           phi-search--last-executed          str)))
  (exit-minibuffer)         ; exit-minibuffer must be called at last
  )

(defun phi-search-abort ()
  "abort phi-search"
  (interactive)
  (phi-search--with-target-buffer
   (phi-search--with-sublimity
    (phi-search--delete-overlays)))
  (let ((phi-search--before-complete-function nil))
    (phi-search-complete)))

;; + provide

(provide 'phi-search-core)

;;; phi-search-core.el ends here
