;;; r-autoyas.el --- Provides automatically created yasnippets for R function argument lists.
;;
;; Filename: r-autoyas.el
;; Description: r-autoyas is a small ESS complement. It provides automatically created yasnippets for R function argument lists.
;; Author: Sven Hartenstein & Matthew Fidler
;; Maintainer: Matthew Fidler
;; Created: Fri Mar 25 10:36:08 2011 (-0500)
;;
;; Version: 0.28
;; Last-Updated: Mon Jun 25 15:12:20 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 873

;; URL: https://github.com/mlf176f2/r-autoyas.el
;; Package-Requires: ((ess "0") (yasnippet "0.8.0"))
;; Keywords: R yasnippet
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 18-Sep-2012      
;;    Last-Updated: Mon Jun 25 15:12:20 2012 (-0500) #873 (Matthew L. Fidler)
;;    Should be compatible with 0.6 and 0.8 versions of Yasnippet and fix
;;    issue #4
;; 17-Sep-2012      
;;    Last-Updated: Mon Jun 25 15:12:20 2012 (-0500) #873 (Matthew L. Fidler)
;;    Should now work with yasnippet 0.8 -- Mostly fixes issue #4, but needs
;;    to confirm backward compatability with 0.6.
;; 17-Sep-2012      
;;    Last-Updated: Mon Jun 25 15:12:20 2012 (-0500) #873 (Matthew L. Fidler)
;;    Added some more fixes to conform to the 0.8 style variables.
;; 13-Sep-2012      
;;    Last-Updated: Mon Jun 25 15:12:20 2012 (-0500) #873 (Matthew L. Fidler)
;;    Did not catch yas--update-mirrors.  Need to fix this.
;; 12-Sep-2012      
;;    Last-Updated: Mon Jun 25 15:12:20 2012 (-0500) #873 (Matthew L. Fidler)
;;    Have attempted to make r-autoyas compatible with yasnippet 0.8.  This
;;    will possibly address github issue #4
;; 04-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Jun  4 15:25:23 2012 (-0500) #865 (Matthew L. Fidler)
;;    Bug fix for autopair-mode
;; 04-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Jun  4 14:37:39 2012 (-0500) #863 (Matthew L. Fidler)
;;    Changed syntax table for yas/expand so that write.csv will
;;    expand if you have a snippet named csv.
;; 04-Jun-2012    Matthew L. Fidler  
;;    Last-Updated: Mon May  7 13:23:33 2012 (-0500) #851 (Matthew L. Fidler)
;;    Bug fix for autopair.
;; 07-May-2012    Matthew L. Fidler  
;;    Last-Updated: Mon May  7 13:01:29 2012 (-0500) #842 (Matthew L. Fidler)
;;    Changed the syntax table for `r-autoyas-expand' so that when a
;;    snippet `csv' is defined and you expand at write.csv, write.csv
;;    will be expanded instead of `csv'
;; 02-Feb-2012    Matthew L. Fidler
;;    Last-Updated: Sat Dec  3 22:20:47 2011 (-0600) #834 (Matthew L. Fidler)
;;    This package no longer auto-loads.
;; 29-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Fri Nov 18 14:35:01 2011 (-0600) #799 (Matthew L. Fidler)
;;    Change the *r-autoyas* buffer to be hidden (ie " *r-autoyas*")
;; 18-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Fri Nov 18 14:34:39 2011 (-0600) #798 (Matthew L. Fidler)
;;    Added gihub URL
;; 17-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Nov 17 13:02:52 2011 (-0600) #795 (Matthew L. Fidler)
;;    Fixed `called-interactively-p' to have a single argument.
;; 17-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Nov 17 10:50:16 2011 (-0600) #790 (Matthew L. Fidler)
;;    Added which to the default ignored parenthetical statements
;; 17-Nov-2011    Matthew L. Fidler
;;    Last-Updated: Thu Nov 17 09:05:49 2011 (-0600) #787 (Matthew L. Fidler)
;;    Fixed `r-autoyas-defined-p'
;; 17-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Nov 17 08:48:03 2011 (-0600) #784 (Matthew L. Fidler)
;;    Added Forward compatablilty for (interactive-p)
;; 17-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Nov 16 14:40:52 2011 (-0600) #782 (Matthew L. Fidler)
;;    Changed the order of r-autoyas alais of old
;; 16-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Nov 16 14:10:32 2011 (-0600) #758 (Matthew L. Fidler)
;;    Changed ignored expressions to only be ignore when using a
;;    parenthesis, and added more ignored expressions
;; 16-Nov-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Nov 16 09:27:31 2011 (-0600) #746 (Matthew L. Fidler)
;;    Updated to have better wrapping after exiting a snippet.
;; 08-Jun-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Jun  8 16:21:09 2011 (-0500) #741 (Matthew L. Fidler)
;;    A partial fix for noweb (Rnw)
;; 06-Jun-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Jun  6 17:07:36 2011 (-0500) #733 (Matthew L. Fidler)
;;    Small update to fix lisp-based replacements.
;; 06-Jun-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Jun  6 15:23:54 2011 (-0500) #725 (Matthew L. Fidler)
;;    Added a bug-fix for complex language statements like reshape.
;; 16-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May 16 19:38:46 2011 (-0500) #718 (Matthew L. Fidler)
;;    Bug Fixes for cached snippets.
;; 16-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May 16 18:27:44 2011 (-0500) #678 (Matthew L. Fidler)
;;    Added wrapping capaibilites to code.  Currently only works on Ctl-G.
;; 16-May-2011    Matthew L. Fidler
;;    Last-Updated: Mon May 16 16:15:25 2011 (-0500) #634 (Matthew L. Fidler)
;;    Added option to remove explicit parameter names for functions if not needed.
;; 16-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May 16 14:02:14 2011 (-0500) #533 (Matthew L. Fidler)
;;    Allow autopair backspace to delete autostarted template.
;; 16-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May 16 10:21:14 2011 (-0500) #443 (Matthew L. Fidler)
;;    Changed language constructs to make sure its not a default text.
;; 16-May-2011    Matthew L. Fidler  
;;    Last-Updated: Mon May 16 09:05:32 2011 (-0500) #434 (Matthew L. Fidler)
;;    Changed quoting method to fix read.table()
;; 16-May-2011    Matthew L. Fidler
;;    Last-Updated: Mon May 16 08:25:04 2011 (-0500) #422 (Matthew L. Fidler)
;;
;;    Removed if (grepl(', ', str, fixed=TRUE)) str <- sub(', ', '', str); from R code to fix write.table()
;;
;; 26-Apr-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Apr 26 09:37:36 2011 (-0500) #417 (Matthew L. Fidler)
;;    Now when using Control-G to exit snippets, it will not delete anything inside the snippet.
;;
;;    For example, using ls(name=".txt|",...) where the cursor is at |,
;;    pressing Cntrl-G
;; 26-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Tue Apr 26 09:13:35 2011 (-0500) #397 (Matthew L. Fidler)
;;    Added a space to try to fix the strange duplication issues.
;; 25-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Mon Apr 25 14:32:16 2011 (-0500) #360 (Matthew L. Fidler)
;;    Bug fix for nested auto-expansion using (.
;; 21-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Thu Apr 21 14:55:20 2011 (-0500) #329 (Matthew L. Fidler)
;;    Tried to fix the autobrackets in r-auotyas.
;; 15-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Fri Apr 15 11:50:41 2011 (-0500) #284 (Matthew L. Fidler)
;;    Bugfix for ess-eval-linewise option
;; 15-Apr-2011    Matthew L. Fidler  
;;    Last-Updated: Fri Apr 15 09:21:01 2011 (-0500) #276 (Matthew L. Fidler)
;;    Fixed autopair bug.
;; 15-Apr-2011    Matthew L. Fidler
;;    Last-Updated: Fri Apr 15 08:46:52 2011 (-0500) #252 (Matthew L. Fidler)
;;    Changed `r-autoyas-inject-commnads' to use `ess-eval-linewise' in mering with Svens' version. (Its an option)
;; 11-Apr-2011    Matthew L. Fidler  
;;    Tried to fix the auto-parenthesis issue in autopair.
;;    
;; 10-Apr-2011      
;;    Added splitting the snippets with returns.
;; 09-Apr-2011      
;;    Added autoload.
;; 09-Apr-2011      
;;    Some bug-fixes to the new yasnippet mechanism.
;; 30-Mar-2011    Matthew L. Fidler

;;    Attempted to allow nested expansion, as well as changing the
;;    mechanism of Yasnippet expansion.

;; 25-Mar-2011    Matthew L. Fidler
;;    Initial release as ELPA package.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ess-site)
(require 'yasnippet nil t)
(require 'yasnippet-bundle nil t)

(defvar r-autoyas-backward-compatability
  '((yas/expand-snippet yas-expand-snippet)
    (yas/active-field-overlay yas--active-field-overlay)
    (yas/wrap-around-region yas-wrap-around-region)
    (yas/moving-away-p yas-moving-away-p)
    (yas/expand yas-expand)
    (yas/modified-p yas-modified-p)     
    (yas/moving-away-p yas-moving-away-p)
    (yas/text yas-text)
    (yas/skip-and-clear-or-delete-char yas-skip-and-clear-or-delete-char)
    (yas/snippet-fields yas--snippet-fields)
    (yas/snippets-at-point yas--snippets-at-point)
    (yas/update-mirrors yas--update-mirrors)
    (yas/fallback-behavior yas-fallback-behavior)
    (yas/minor-mode yas-minor-mode)
    (yas/field-probably-deleted-p yas--field-probably-deleted-p)
    (yas/field yas--field)
    (yas/field-text-for-display yas--field-text-for-display)
    (yas/snippet-control-overlay yas--snippet-control-overlay)
    (yas/exit-snippet yas-exit-snippet)
    (yas/check-commit-snippet yas--check-commit-snippet)
    (yas/define-snippets yas-define-snippets)
    (yas/after-exit-snippet-hook yas-after-exit-snippet-hook)
    )
  "Yasnippet backward compatability functions used in r-autoyas.el")

(defvar r-autoyas-backward nil
  "R-autoyas use backward compatability?")
;; Add backward compatability when needed.
(mapc
 (lambda(what)
   (when (not (eval `(or (fboundp ',(nth 1 what)) (boundp ',(nth 1 what)))))
     (if (eval `(functionp ',(nth 0 what)))
         (progn
           (setq r-autoyas-backward t)
           (eval `(defalias ',(nth 1 what) ',(nth 0 what))))
       (eval `(defvaralias ',(nth 1 what) ',(nth 0 what))))))
 r-autoyas-backward-compatability)

(defgroup r-autoyas nil
  "R auto Yasnippet generation."
  :group 'yasnippet)

(defcustom r-autoyas-debug nil
  "Add debugging comments for`r-autoyas'"
  :type 'boolean
  :group 'r-autoyas)

(defcustom r-autoyas-use-r-based-dot-replacement t
  "Uses Lisp-based dot-replacement defined by `r-autoyas-r-based-dot-replacement' instead of specifying through options in R startup."
  :type 'boolean
  :group 'r-autoyas)

(defcustom r-autoyas-r-based-dot-replacement nil
  "Defines default the ... replacement sent to the options() statement in R. quote() is used to keep the expression instead of evaluating it."
  :type '(repeat
          (list
           (string :tag "Function to replace `...'")
           (repeat
            (string :tag "Extra argument: "))))
  :group 'r-autoyas)

(defcustom r-autoyas-use-lisp-based-dot-replacement t
  "Uses Lisp-based dot-replacement defined by `r-autoyas-lisp-based-dot-replacement' instead of specifying through options in R startup."
  :group 'r-autoyas
  :type 'boolean)

(defcustom r-autoyas-save-expression-to-memory t
  "Defines if r-autoyas should save the snippet to memory instead of calling the R communcation again."
  :group 'r-autoyas
  :type 'boolean)

(defcustom r-autoyas-expand-package-functions-only t
  "Automatically expand only functions defined in a package/library."
  :group 'r-autoyas
  :type 'boolean)

(defcustom r-autoyas-preloaded-packages
  '("base" "graphics" "grDevices" "methods" "stats" "tools" "utils")
  "List of preloaded packages in R.  All other packages need to be included by a require(package) or library(package) statement."
  :type '(repeat (string :tag "Package Name"))
  :group 'r-autoyas)

(defcustom r-autoyas-paren-ignored-functions
  '("function" "for" "if" "cos" "sin" "exp" "tan" "data.frame" "cat" "print" "seq" "rbind" "cbind" "which" "duplicated")
  "List of functions to ignore when creating auto-snippets by inserting a parenthesis"
  :type '(repeat (string :tag "Ignored R function"))
  :group 'r-autoyas)

(defalias 'r-autoyas-ignored-functions r-autoyas-paren-ignored-functions)

(defcustom r-autoyas-number-of-commas-before-return 4
  "Defines the number of commas before the snippet is inserted as:

plot(x= ,
     y=NULL,
     type='p',
     xlim=NULL,
     ylim=NULL,
     log='',
     main=NULL,
     sub=NULL,
     xlab=NULL,
     ylab=NULL,
     ann=par(\"ann\"),
     axes=TRUE,
     frame.plot= ,
     panel.first=NULL,
     panel.last=NULL,
     asp=NA, ...)

insetad of:

plot(x= , y=NULL, type='p', xlim=NULL, ylim=NULL, log='', main=NULL, sub=NULL, xlab=NULL, ylab=NULL, ann=par(\"ann\"), axes=TRUE, frame.plot= , panel.first=NULL, panel.last=NULL, asp=NA, ...)

If this number is zero or below, always insert as a single line.

"
  :type 'integer
  :group 'r-autoyas)

(defcustom r-autoyas-remove-explicit-assignments t
  "* Remove explicit assignments when appropriate.

This option removes explicit assignments after tabbing away.  For example

write.table(x= ,
            file=\"\",
            append=FALSE,
            quote=TRUE,
            sep=\" \",
            eol=\"\\n\",
            na=\"NA\",
            dec=\".\",
            row.names=TRUE,
            col.names=TRUE,
            qmethod=c(\"escape\", \"double\"))

Becomes

write.table(d,
            \"file-name.csv\",
            append=FALSE,
            quote=TRUE,
            sep=\" \",
            eol=\"\\n\",
            na=\"NA\",
            dec=\".\",
            row.names=TRUE,
            col.names=TRUE,
            qmethod=c(\"escape\", \"double\"));

"
  :type 'boolean
  :group 'r-autoyas)

(defvar r-autoyas-cache nil
  "Cache of complex language statments for R-autoyas")

(defcustom r-autoyas-wrap-on-exit t
  "Defines if R-autoyas attempts to wrap end of lines."
  :type 'boolean
  :group 'r-autoyas)

(defcustom r-autoyas-lisp-based-dot-replacement
  '(("xyplot" (
               "allow.multiple = quote(is.null(groups) || outer)"
               "outer = quote(!is.null(groups))"
               "auto.key = FALSE"
               "aspect = \"fill\""
               "panel = quote(lattice.getOption(\"panel.xyplot\"))"
               "prepanel = NULL"
               "scales = quote(list())"
               "strip = TRUE"
               "groups = NULL"
               "xlab"
               "xlim"
               "ylab"
               "ylim"
               "drop.unused.levels = lattice.getOption(\"drop.unused.levels\")"
               "lattice.options = NULL"
               "default.scales"
               "subscripts = !is.null(groups)"
               "subset = TRUE"))
    ("write.csv" (
                  "x= "
                  "file=\"\""
                  "quote=TRUE"
                  "eol=\"\\n\""
                  "na=\"NA\""
                  "row.names=TRUE"))
    ("bwplot" (
               "allow.multiple = quote(is.null(groups) || outer)"
               "outer = FALSE"
               "auto.key = FALSE"
               "aspect = \"fill\""
               "panel = quote(lattice.getOption(\"panel.bwplot\"))"
               "prepanel = NULL"
               "scales = list()"
               "strip = TRUE"
               "groups = NULL"
               "xlab"
               "xlim"
               "ylab"
               "ylim"
               "box.ratio = 1"
               "horizontal = NULL"
               "drop.unused.levels = quote(lattice.getOption(\"drop.unused.levels\"))"
               "lattice.options = NULL"
               "default.scales"
               "subscripts = quote(!is.null(groups))"
               "subset = TRUE")))
  "Defines default the ... replacement using lisp."
  :type '(repeat
          (list
           (string :tag "Function to replace `...'")
           (repeat
            (string :tag "Extra argument: "))))
  :group 'r-autoyas)

(defvar rayas-last-active nil)
(make-variable-buffer-local 'rayas-last-active)

(defun rayas-require-explicit-p (num)
  "Should the explicit x= be required?"
  ;; Checks to see if the explicit x= is required.
  (let ((i (- num 1))
	(ret nil)
        (snippet (if yas-snippets (first yas-snippets) nil))
        (fields (if snippet (yas--snippet-fields snippet) nil))
        (anum (r-autoyas-active-field-number) ))
    (when anum
      (setq rayas-last-active anum))
    (if (not anum)
	(if rayas-last-active
	    (setq anum (+ 1 rayas-last-active))))
    (cond
     ((not snippet)
      (setq rayas-last-active nil)
      (setq ret t))
     ( (and anum (>= (- num 1) anum) )
       (setq ret t))
     (t
      (while (<= 0 i)
	(when fields
	  (when (string= "" (yas--field-text-for-display (nth i fields)))
	    (setq ret t)))
	(setq i (- i 1)))))
    (symbol-value 'ret)))

(defun rayas-comma (field num)
  "Inserts comma and field number if needed"
  (let* ((comma-text yas-text)
         (yas-snippets (yas--snippets-at-point))
	 (snippet (if yas-snippets (first yas-snippets) nil)) ;; Check to see if this is the first "comma" statement when editing snippet.
	 (control-overlay (if snippet (yas--snippet-control-overlay snippet)))
	 n-space
	 snippet-end
	 (snippet-beg (and control-overlay (overlay-buffer control-overlay)
			   (overlay-start control-overlay))))
    (if (and (string= yas-text "")
	     (r-autoyas-editing-field-num-p (- num 1)))
	(setq comma-text " "))
    (concat
     (cond
      ((and (boundp 'function-name) ;; When initially creating snippet.
	    (= 1 num)
	    function-name
	    (looking-back (format "%s(" function-name)))
       "")
      ((string= "" comma-text) ; Deleted text.
       "")
      ((if (not snippet-beg) nil
	 (string-match "($" (buffer-substring-no-properties snippet-beg (point))))
       "")
      (t
       (if (and (boundp 'n-comma);; Initial construction
		(< 0 r-autoyas-number-of-commas-before-return)
		(>= n-comma r-autoyas-number-of-commas-before-return))
           (progn
	     (if (= 1 num) "" ",\n"))
	 (if (not snippet-beg) (if (= 1 num) "" ", ") ;; Can't figure out the snippet beginning.
	   (setq snippet-end (overlay-end control-overlay))
	   (save-excursion
	     (goto-char snippet-beg)
	     (if (not (search-forward ",\n" snippet-end t)) ", " ;Cant find return use ", "
	       (goto-char snippet-beg)
	       (if (not (search-forward "(" nil t)) (if (= 1 num) "" ", ")
		 (save-restriction
		   (widen)
		   (setq n-space (current-column)))
		 (if (= 1 num) ""
                   (concat ",\n" (make-string n-space ? ))))))))))
     (if (and field (not (string= "" comma-text)))
         (if (or (not r-autoyas-remove-explicit-assignments) (rayas-require-explicit-p num))
             (concat field "=")
	   "")
       ""))))

(defcustom r-autoyas-echo-inject-commands nil
  "* When true use `ess-eval-linewise' to echo the commands to the R process.  Otherwise use `ess-command' to quietly add the lines to the R process."
  :type 'boolean
  :group 'r-autoyas)

(defcustom r-autoyas-auto-expand-with-paren nil
  "* When true will automatically expand with the ( key."
  :type 'boolean
  :group 'r-autoyas)

(defun r-autoyas-m (&rest objects)
  "Message when debugging is on."
  (when r-autoyas-debug
    (apply 'message objects)))

;;${3:$(if (string= "" text) "" ", ")}${3:...$(yas/ma "")}
(defun r-autoyas-generte-dotreplace-list-lisp (func)
  "Generates dot-replacement yasnippet based on lisp options"
  (r-autoyas-m "Calling `r-autoyas-generate-dotreplace-list-lisp' %s" func)
  (when r-autoyas-use-lisp-based-dot-replacement
    (when (assoc func r-autoyas-lisp-based-dot-replacement)
      (r-autoyas-m "Found a lisp replacement for %s" func)
      (goto-char (point-min))
      (let ((num 0)
            (snip ""))
	(save-excursion
	  (goto-char (point-min))
	  (when (re-search-forward
		 "\\${\\([0-9]+\\):\\$(rayas-comma .*?)}\\${\\1:...\\$(rayas-ma \"\")}" nil t)
	    (save-match-data
	      (setq num (string-to-number (match-string 1)))
	      (setq snip
		    (mapconcat
		     (lambda(x)
		       (prog1
			   (if (string-match "^[ \t]*\\(.*?\\)[ \t]*=[ \t]*\\(.*?\\)[ \t]*$" x)
			       (progn
				 (format "${%s:$(rayas-comma \"%s\" %s)}${%s:%s$(rayas-ma \"\")}"
					 num (match-string 1 x) num num (if (< 0 (length (match-string 2 x))) (match-string 2 x) " ")))
			     (format "${%s:$(rayas-comma \"%s\" %s)}${%s:NULL$(rayas-ma \"\")}" num x num num))
                         (setq num (+ num 1))))
		     (nth 1 (assoc func r-autoyas-lisp-based-dot-replacement))
		     "")))
	    (replace-match (format "%s${%s:$(rayas-comma nil %s)}${%s:...$(rayas-ma \"\")}" snip num num num) t t)))
	(r-autoyas-m "Snippet: %s" snip)
	(symbol-value 'snip)))))

(defun r-autoyas-generate-dotreplace-list ()
  "Generates dot-replace R-code"
  (r-autoyas-m "Calling `r-autoyas-generate-dotreplace-list'")
  (when r-autoyas-use-r-based-dot-replacement
    (let ((ret
           (concat "options(r.autoyas.dotreplace=list("
                   (mapconcat
                    (lambda(x)
                      (concat (nth 0 x) "=list("
                              (mapconcat
                               (lambda(y)
                                 (if (string-match "=" y)
                                     y
                                   (concat y "=NULL")))
                               (nth 1 x)
                               ",")
                              ")"))
                    r-autoyas-r-based-dot-replacement ",") "));\n")))
      (ess-command ret ))))

(defun r-autoyas-exit-snippet-delete-remaining ()
  "Exit yas snippet and delete the remaining argument list."
  (interactive "*")
  (r-autoyas-m "Call `r-autoyas-exit-snippet-delete-remaining'")
  (r-autoyas-update)
  (let ((deletefrom (point)))
    (yas-exit-snippet (nth 0 (yas--snippets-at-point)))
    (yas--check-commit-snippet)
    (delete-region (save-excursion
                     (goto-char deletefrom)
		     (or (re-search-backward "[,(][^,(]*[ \t]*=[ \t]*\\=" nil t)
			 (re-search-backward "[,(][ \t]*\\=" nil t))
		     (when (not (looking-at "[ \t]*[,()]"))
		       (when (re-search-forward "[,)]" nil t)
			 (backward-char 1)
			 (skip-chars-backward " \t\n")))
		     (skip-chars-forward "(")
		     (point)) (- (point) 1))
    (when r-autoyas-wrap-on-exit
      (r-autoyas-wrap))))

(defun r-autoyas-wrap ()
  "Wrap code"
  (interactive)
  (r-autoyas-m "Calling `r-autoyas-wrap'")
  (when (looking-back ")")
    (let ((pt (point)))
      (save-excursion
	(backward-sexp)
	(while (re-search-forward ",[ \t]*\n[ \t]*\\(.*?\\)[ \t]*$" nil t)
	  (goto-char (match-beginning 0))
	  (if (< fill-column (+ (current-column) (length (match-string 1))))
	      (progn
		(goto-char (match-end 0))
		(beginning-of-line))
	    (replace-match ", \\1")
	    (skip-chars-backward ",")))))))

(defun r-autoyas-expand (&optional rm-paren)
  "Insert argument list for R function before the point as intelligent yas snippets and
expand the snippets.
RM-PAREN removes the inserted parenthesis"
  (interactive "*")
  (r-autoyas-m "Calling `r-autoyas-expand'")
  (modify-syntax-entry ?. "w")
  (save-match-data
    (save-restriction
      (widen)
      (let ((funcname (r-autoyas-defined-p rm-paren))
            (snippet "")
            namespace
            ret
            not-valid
            n-comma)
        (r-autoyas-m "Function to expand: %s" funcname)
        (if (not funcname) nil
          (r-autoyas-m "Starting to create snippet")
          (ess-command (concat ".r.autoyas.create('" funcname "')\n")
                       (get-buffer-create " *r-autoyas*"))
          (r-autoyas-m "Created snippet in ` *r-autoyas*'")
          (unless (null funcname)
            (let (snippet)
              (save-excursion
                (with-current-buffer " *r-autoyas*"
                  (r-autoyas-m "Trying to extract snippet.")
                  (if (< (length (buffer-string)) 10);; '[1] " "' if no valid fun
                      (progn
                        (message "function `%s' is not valid!" funcname)
                        (setq not-valid t)
                        (when (insert "("))
                        (setq ret t))
                    (delete-region 1 6)
                    (goto-char (point-max))
                    (delete-backward-char 2)
                    (goto-char (point-min))
                    (replace-string "\\\"" "\"")
                    (goto-char (point-min))
                    (replace-string "\\\\" "\\")
                    (r-autoyas-generte-dotreplace-list-lisp funcname)
                    (setq snippet (buffer-string)))))
              (if not-valid
                  (progn
                    (setq ret nil))
                (save-excursion 
                  (when (and rm-paren (looking-back "("))
                    (replace-match "")))
                (setq namespace (r-autoyas-namespace funcname))
                (r-autoyas-m "R-autoyas assumed the namespace for the function is: `%s'" namespace)
                (when (or
                       (and r-autoyas-expand-package-functions-only namespace)
                       (not r-autoyas-expand-package-functions-only))
                  (if snippet
                      (progn
                        (setq n-comma (- (length (split-string snippet (regexp-quote "(rayas-comma") t)) 1))
                        (if (or (not namespace) (not r-autoyas-save-expression-to-memory))
                            (let ((function-name funcname)
                                  (n-comma n-comma))
                              (r-autoyas-m "R-autoyas expanding snippet but not saving to memory")
                              (yas-expand-snippet snippet)
                              (setq ret t))
                          (r-autoyas-m "R-autoyas saving snippet to `ess-mode'")
                          (yas-define-snippets 'ess-mode
                                               `((,(format "%s" funcname)
                                                  ,(concat funcname snippet)
                                                  ,(format "%s" funcname)
                                                  "(string= ess-dialect \"R\")"
                                                  nil
                                                  ,(format
                                                    "((function-name \"%s\") (n-comma %s))"
                                                    funcname n-comma))))
                          (yas-expand)
                          (setq ret t)))
                    (setq ret t))))))
          (symbol-value 'ret)))))
  (modify-syntax-entry ?. "_"))

(defun rayas-space (field-number)
  "Adds a dummy space so that reducing the yasnippet field to zero doesn't cause strange errors."
  (condition-case err
      (cond
       (yas-moving-away-p
	"")
       ((r-autoyas-editing-field-num-p (- field-number 1))
	" ")
       (t ""))
    (error " ")))

(defun r-autoyas-inject-commands ()
  (interactive)
  (r-autoyas-m "Injecting `r-autoyas-inject-commands'")
  (let ((cmd "if (!any(ls(all=TRUE) == \".r.autoyas.create\")){
.r.autoyas.esc <- function(str) {
str <- gsub('$', '\\\\$', str, fixed=TRUE);
str <- gsub('`', '\\\\`', str, fixed=TRUE);
return(str);
};

.r.autoyas.print <- function(funcname) {
if (existsFunction(deffun <- paste(funcname,'.default', sep=''))) {
funcname <- deffun;
} else if(!existsFunction(funcname)) {
return(' ');
}
print(eval(parse(text=funcname)))
}

.r.autoyas.create <- function(funcname) {
if (existsFunction(deffun <- paste(funcname,'.default', sep=''))) {
funcname <- deffun;
} else if(!existsFunction(funcname)) {
return(' ');
}
formals <- formals(funcname);
dots <- match('...', names(formals));
if (!is.na(dots) & !is.null(options()[['r.autoyas.dotreplace']][[funcname]])) {
formals2 <- NULL;
if (dots > 1) formals2 <- formals[1:(dots-1)];
formals2 <- append(formals2, options()[['r.autoyas.dotreplace']][[funcname]]);
if (dots < length(formals)) formals2 <- append(formals2, formals[(dots+1):length(formals)]);
formals <- formals2;
}
nr <- 1;
closebrackets <- 0;
str <- NULL;
for (field in names(formals)) {
type <- typeof(formals[[field]]);
if (type=='symbol' & field!='...') {
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr ,':',' $(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='symbol' & field=='...') {
str <- append(str, paste('${',nr,':$(rayas-comma nil ',nr,')}${',nr,':',field,'$(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='character') {
tmp <- .r.autoyas.esc(encodeString(formals[[field]]))
tmp <- gsub(\"\\\"\",\"\\\\\\\\\\\\\\\"\",tmp);
tmp <- paste(\"\\\"\",tmp,\"\\\"\",sep=\"\");
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',tmp,'$(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='logical') {
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',as.character(formals[[field]]),'$(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='double') {
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',as.character(formals[[field]]),'$(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='NULL') {
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':NULL$(rayas-ma \\\"\\\")}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else if (type=='language') {
tmp <- deparse(formals[[field]]);
if (all(regexpr(\"[{}\\n]\", tmp) == -1)){
tmp <- .r.autoyas.esc(tmp);
#tmp2 <- gsub(\"\\\"\",\"\\\\\\\\\\\\\\\"\",tmp);
tmp2 <- gsub(\"\\\"\",\"%'%\",tmp);
tmp2 <- paste(\"\\\"\",tmp2,\"\\\"\",sep=\"\");
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',tmp,'$(rayas-ma \"\" ',tmp2,')}${',nr,':$(rayas-space ',nr,')}', sep=''));
nr <- nr+1;
} else {
tmp <- .r.autoyas.esc(tmp)
tmp2 <- paste(gsub(\"\\\"\",\"\\\\\\\\\\\\\\\"\",tmp),collapse=\"\");
tmp2 <- paste(\"\\\"\",tmp2,\"\\\"\",sep=\"\");
if (length(str) > 1){
str[1] <-  paste('`(progn (add-to-list \\'r-autoyas-cache \\'((',funcname,' ',nr,') ',tmp2,')) \"\")`', str[1],sep=\"\");
tmp <- paste(\"(cdr (assoc '(\",funcname,\" \",nr,\") r-autoyas-cache))\",sep=\"\")
tmp2 <- paste(\"`\",tmp,\"`\",sep=\"\")
str <- append(str, paste('${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',tmp2,'$(rayas-ma \"\" ',tmp,')}${',nr,':$(rayas-space ',nr,')}', sep=''));
} else {
tmp <- paste(\"(cdr (assoc '(\",funcname,\" \",nr,\") r-autoyas-cache))\",sep=\"\")
tmp3 <- paste(\"`\",tmp,\"`\",sep=\"\")
str <- append(str, paste('`(progn (add-to-list \\'r-autoyas-cache \\'((',funcname,' ',nr,') ',tmp2,')) \"\")`','${',nr,':$(rayas-comma \\\"',field,'\\\" ',nr,')}${',nr,':',tmp3,'$(rayas-ma \"\" ',tmp,')}${',nr,':$(rayas-space ',nr,')}', sep='')); 
}
nr <-  nr+1;
}
}
}
str <- paste(str, sep='', collapse='');
##if (grepl(', ', str, fixed=TRUE)) str <- sub(', ', '', str);
str <- paste('(',str,')', sep='');
return(str);
}}
cat(\"Loaded r-autoyas\\n\");
"))
    (cond
     (r-autoyas-echo-inject-commands
      (ess-eval-linewise cmd))
     (t
      (ess-command cmd)))
    
    (r-autoyas-generate-dotreplace-list)))

(defadvice yas/abort-snippet (around r-delete-remaining)
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (r-autoyas-exit-snippet-delete-remaining))
  ad-do-it)

(ad-activate 'yas/abort-snippet)

(defadvice yas-abort-snippet (around r-delete-remaining)
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (r-autoyas-exit-snippet-delete-remaining))
  ad-do-it)
(ad-activate 'yas-abort-snippet)


(add-hook 'ess-post-run-hook (lambda ()
                               (if (string= ess-dialect "R")
                                   (r-autoyas-inject-commands))))

(defadvice noweb-indent-line (around r-autoyas-update)
  "Allow noweb files to have R-autoyas enabled"
  (let (do-it)
    (if (interactive-p)
	(if (and (boundp 'ess-dialect)
		 (string= ess-dialect "R"))
	    (if (not (r-autoyas-expand))
		(setq do-it t))
	  (setq do-it t))
      (setq do-it t))
    (when do-it
      ad-do-it)))

(ad-activate 'noweb-indent-line)

(defun r-autoyas-expand-maybe (&rest ignore)
  "Might auto-expand snippet."
  (interactive)
  (r-autoyas-m "Calling `r-autoyas-expang-maybe'")
  (when (string= ess-dialect "R")
    (if (not (r-autoyas-expand))
	(call-interactively 'ess-indent-command))))

(defun r-autoyas-namespace (function-name)
  "Returns the namespace for FUNCTION-NAME, or nil if it cannot be determined."
  (let ((namespace nil))
    (r-autoyas-m "Trying to determine the namespace for %s with `r-autoyas-namespace'" function-name)
    (ess-command (concat "print(" function-name ")\n")
                 (get-buffer-create " *r-autoyas*"))
    (save-excursion
      (with-current-buffer " *r-autoyas*"
	(goto-char (point-max))
	(when (re-search-backward "<environment:[ \t]*namespace:\\(.*?\\)>" nil t)
	  (setq namespace (match-string 1)))))
    (unless namespace
      ;; Look for XXX.default
      (ess-command (concat "print(" function-name ".default)\n")
                   (get-buffer-create " *r-autoyas*"))
      (save-excursion
        (with-current-buffer " *r-autoyas*"
          (goto-char (point-max))
          (when (re-search-backward "<environment:[ \t]*namespace:\\(.*?\\)>" nil t)
            (setq namespace (match-string 1))))))
    (symbol-value 'namespace)))

(defun r-autoyas-preloaded-namespace-p (namespace)
  "Determines if NAMESPACE is preloaded in R.  It is based on the variable `r-autoyas-preloaded-packages'"
  (r-autoyas-m "Called `r-autoyas-preloaded-namespace-p'")
  (memq namespace r-autoyas-preloaded-packages))

(defun r-autoyas-defined-p (&optional with-paren)
  "Is the current function defined (like plot )"
  (interactive (list (yes-or-no-p "Paren?")))
  (r-autoyas-m "Trying to figure if the current function is defined")
  (save-restriction
    (save-excursion 
      (widen) ;; Widen needed for autopair mode.
      (let (ret tmp)
	(when (looking-back
	       (concat "\\(?:\\[\\|\\]\\|[^\n\t ()\"={}|\/<>:;'`'!@#$%^&*-+]\\)*"
		       (if with-paren "(" "")) nil t)
	  (setq tmp (match-string 0))
	  (if  (string= "(" tmp)
	      (setq ret nil)
	    (when with-paren
	      (setq tmp (substring tmp 0 -1)))
            (ess-command (concat "existsFunction(\"" tmp "\");\n")
                         (get-buffer-create " *r-autoyas*"))
            (with-current-buffer " *r-autoyas*"
              (goto-char (point-min))
              (when (save-match-data (search-forward " TRUE" nil t))
                (setq ret t))))
          (when ret
            (setq ret tmp))
          (when (and ret with-paren (member ret r-autoyas-paren-ignored-functions))
            (setq ret nil))
          (when (or (and (fboundp 'interactive-p) (interactive-p))
                    (and (fboundp 'called-interactively-p) (called-interactively-p t)))
            (r-autoyas-m "Defined: %s" ret))
          (symbol-value 'ret))))))

;;;###autoload
(defun r-autoyas-ess-activate ()
  "R autoyas ESS hook"
  (interactive)
  (when (featurep 'r-autoyas)
    (set (make-local-variable 'yas-fallback-behavior)
         '(apply r-autoyas-expand-maybe))
    (when (boundp 'autopair-handle-action-fns)
      (set (make-local-variable 'autopair-handle-action-fns)
           (list                        
            #'autopair-r-autoyas-paren-action)))
    (yas-minor-mode 1)
    (when (boundp 'yas-after-exit-snippet-hook)
      (add-hook 'yas-after-exit-snippet-hook
                (lambda()
                  (interactive)
                  (when r-autoyas-wrap-on-exit
                    (r-autoyas-wrap))) t t))))

(defvar r-autoyas-paren-skip-autopair nil)
(make-variable-buffer-local 'r-autoyas-paren-skip-autopair)

(defun r-autoyas-paren ()
  "Function to allow Auto-yas to insert parenthesis"
  (interactive)
  (r-autoyas-m "Called `r-autoyas-paren'")
  (let ((r-autoyas-using-paren t))
    (if (and
         r-autoyas-auto-expand-with-paren
         (boundp 'skeleton-pair)
         skeleton-pair
         (r-autoyas-defined-p))
        (progn
          (if (not (r-autoyas-expand))
              (progn
                (self-insert-command 1)
                (setq this-command 'self-insert-command))))
      (if (and (boundp 'skeleton-pair)
               skeleton-pair)
          (skeleton-pair-insert-maybe nil)
        (self-insert-command 1)
        (setq this-command 'self-insert-command)))))

(defun autopair-r-autoyas-paren-action (action pair pos-before)
  "Autopair R autoyas paren-action"
  (if (string= ess-dialect "R")
      (condition-case err
          (let ((yas-wrap-around-region yas-wrap-around-region)
                (ret (and
		      r-autoyas-auto-expand-with-paren
		      (eq action 'opening)
		      (= pair 41)
		      (r-autoyas-defined-p t)))
                (pt (point)))
	    (if (not ret)
                (autopair-default-handle-action action pair pos-before)
              (when (eq yas-wrap-around-region 'cua)
                ;;TODO: Fix this to work with CUA-type wrapping
                (setq yas-wrap-around-region nil))
              (r-autoyas-expand t)
              (message "%s,%s" pt (point))
              (when (= (- pt 1) (point))
                (insert "(")
                (autopair-default-handle-action action pair pos-before))
              (when (= pt (point))
                (autopair-default-handle-action action pair pos-before))))
        (error (message "[r-autoyas-pair-error]: %s" (error-message-string err))))
    (autopair-default-handle-action action pair pos-before)))

(when (boundp 'ess-mode-map)
  (define-key ess-mode-map (kbd "(") 'r-autoyas-paren))

(defun r-autoyas-active-field-number (&optional arg)
  "Get the active field position"
  (if (boundp 'r-autoyas-not-editing)
      nil
    (let* ((arg (or arg  0))
           (snippet (first (yas--snippets-at-point)))
           (active-field (if snippet (overlay-get yas--active-field-overlay 
                                                  (if r-autoyas-backward
                                                      'yas/field
                                                    'yas--field)) nil))
           (live-fields (if (not snippet) nil
                          (remove-if #'
                           (lambda (field)
                             (and (not (eq field active-field))
                                  (yas--field-probably-deleted-p snippet field)))
                                     (yas--snippet-fields snippet))))
           (active-field-pos (if (not snippet) nil (position active-field live-fields))))
      (if (not snippet) nil
        active-field-pos))))

(defun r-autoyas-editing-field-num-p (&optional arg)
  "Which field is active?"
  (if arg
      (let ((active-field-pos (r-autoyas-active-field-number)))
        (if active-field-pos
            (= active-field-pos arg)
          nil))
    nil))

(defun r-autoyas-update ()
  "Update fields"
  (let ((snippet (first (yas--snippets-at-point))))
    (when snippet
      (yas--update-mirrors snippet))))

(defun r-autoyas-text-on-moving-away (default-text &optional orig-text)
  "* Changes text when moving away AND original text has not changed" ;
  (let ((dtxt (replace-regexp-in-string "%'%" "\"" default-text nil t))
        (otxt (if orig-text
                  (replace-regexp-in-string "%'%" "\"" orig-text nil t)
                nil)))
    (cond
     ((or (and (not yas-modified-p) yas-moving-away-p)
          (and yas-moving-away-p otxt (string= otxt yas-text)))
      (let (r-autoyas-not-editing)
        (if (string= "" dtxt)
            (yas-skip-and-clear-or-delete-char)
          (insert dtxt))
        (r-autoyas-update))))))

(defadvice autopair-backspace (around r-autoyas-update)
  "Allows a backspace at the first to remove the autoexpanded snippet."
  (let ((do-it nil))
    (when (and autopair-mode
               (eq major-mode 'ess-mode)
               (string= ess-dialect "R")
               (r-autoyas-editing-field-num-p 0)
               (looking-back "([^(\n]*="))
      (r-autoyas-exit-snippet-delete-remaining)
      (setq do-it t))
    ad-do-it
    (when do-it
      (when (looking-back "(")
        (replace-match "")))))

(ad-activate 'autopair-backspace)

(defadvice yas/next-field (around r-autoyas-update)
  "Updates fields upon [TAB] for r-autoyas-snippets."
  ad-do-it
  (let (r-autoyas-not-editing)
    (r-autoyas-update)))

(defadvice yas-next-field (around r-autoyas-update)
  "Updates fields upon [TAB] for r-autoyas-snippets."
  ad-do-it
  (let (r-autoyas-not-editing)
    (r-autoyas-update)))

(defadvice yas/skip-and-clear-or-delete-char (around r-autoyas-update)
  "Updates fields upon C-d for r-autoyas-snippets."
  ad-do-it
  (let (r-autoyas-not-editing)
    (r-autoyas-update)))

(defadvice yas-skip-and-clear-or-delete-char (around r-autoyas-update)
  "Updates fields upon C-d for r-autoyas-snippets."
  ad-do-it
  (let (r-autoyas-not-editing)
    (r-autoyas-update)))

(defadvice yas/expand-from-trigger-key (around r-autoyas-expand)
  "Changes Syntax table to allow proper expansion in R"
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (modify-syntax-entry ?. "w"))
  ad-do-it
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (modify-syntax-entry ?. "_")))

(defadvice yas-expand-from-trigger-key (around r-autoyas-expand)
  "Changes Syntax table to allow proper expansion in R"
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (modify-syntax-entry ?. "w"))
  ad-do-it
  (if (and (member major-mode '(ess-mode inferior-ess-mode))
           (string= "R" ess-dialect))
      (modify-syntax-entry ?. "_")))

(ad-activate 'yas/next-field)
(ad-activate 'yas/skip-and-clear-or-delete-char)
(ad-activate 'yas/expand-from-trigger-key)

(ad-activate 'yas-next-field)
(ad-activate 'yas-skip-and-clear-or-delete-char)
(ad-activate 'yas-expand-from-trigger-key)

(defalias 'rayas-ma 'r-autoyas-text-on-moving-away)

(provide 'r-autoyas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; r-autoyas.el ends here
