;;; adaptive-wrap.el --- Smart line-wrapping with wrap-prefix

;; Copyright (C) 2011-2013  Free Software Foundation, Inc.

;; Author: Stephen Berman <stephen.berman@gmx.net>
;;         Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `adaptive-wrap-prefix-mode' minor mode which sets
;; the wrap-prefix property on the fly so that single-long-line paragraphs get
;; word-wrapped in a way similar to what you'd get with M-q using
;; adaptive-fill-mode, but without actually changing the buffer's text.

;;; Code:

(defun adaptive-wrap-fill-context-prefix (beg end)
  "Like `fill-context-prefix', but with length adjusted by `adaptive-wrap-extra-indent'."
  ;; Note: fill-context-prefix may return nil; See: http://article.gmane.org/gmane.emacs.devel/156285
  (let* ((fcp (or (fill-context-prefix beg end) ""))
         (fcp-len (string-width fcp))
         (fill-char (if (< 0 fcp-len)
                        (string-to-char (substring fcp -1))
                      ?\s)))

    ;; Tabs are wonky because visual-lines can't work around Emacs' limitation that tabs have to snap to physical columns, so we replace them with spaces instead.
    (setq fcp (replace-regexp-in-string
               "[\t]" (make-string tab-width ?\s)
               fcp))

    ;; Make sure we don't over-indent the line.
    (setq fcp (substring fcp 0
                         (min (window-width) 20 (length fcp))))

    fcp))

(defun adaptive-wrap-prefix-function (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while (< (point) end)
    (let ((lbp (line-beginning-position)))
      (put-text-property (point)
                         (progn (search-forward "\n" end 'move) (point))
                         'wrap-prefix
                         (adaptive-wrap-fill-context-prefix lbp (point))))))

(defun adaptive-wrap-strip-string-for-kill-ring (string &optional replace)
  "Remove wrap prefix from kill-ring because it causes crashes. Yes, seriously."
  (remove-text-properties 0 (length string)
                          '(wrap-prefix nil)
                          string))

;;;###autoload
(define-minor-mode adaptive-wrap-prefix-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  :group 'visual-line
  (if adaptive-wrap-prefix-mode
      (progn
        (jit-lock-register #'adaptive-wrap-prefix-function)
        (advice-add 'kill-new :before #'adaptive-wrap-strip-string-for-kill-ring))

    (jit-lock-unregister #'adaptive-wrap-prefix-function)
    (advice-remove 'kill-new #'adaptive-wrap-strip-string-for-kill-ring)
    (with-silent-modifications
      (save-restriction
        (widen)
        (remove-text-properties (point-min) (point-max) '(wrap-prefix nil))))))

(define-key-after (lookup-key menu-bar-options-menu [line-wrapping])
  [adaptive-wrap]
  '(menu-item "Adaptive Wrap" adaptive-wrap-prefix-mode
	      :visible (menu-bar-menu-frame-live-and-visible-p)
	      :help "Show wrapped long lines with an adjustable prefix"
	      :button (:toggle . (bound-and-true-p adaptive-wrap-prefix-mode)))
  word-wrap)

(defun turn-on-adaptive-wrap-prefix-mode ()
  ;; disable it in org-mode 'cause they already do something like that
  (unless (memq major-mode '(org-mode))
    (adaptive-wrap-prefix-mode 1)))

(define-globalized-minor-mode global-adaptive-wrap-prefix-mode
  adaptive-wrap-prefix-mode turn-on-adaptive-wrap-prefix-mode
  :lighter "")

;;;; ChangeLog:

;; 2013-07-31  Stephen Berman  <stephen.berman@gmx.net>
;;
;; 	* adaptive-wrap.el: Fix bug#14974 by using define-key-after
;; 	instead of easy-menu-add-item.
;; 	(adaptive-wrap-unload-function): Remove.
;;
;; 2013-07-29  Stephen Berman  <stephen.berman@gmx.net>
;;
;; 	* adaptive-wrap.el: Require easymenu (bug#14974).
;;
;; 2013-07-19  R?diger Sonderfeld  <ruediger@c-plusplus.de>
;;
;; 	* adaptive-wrap.el (menu-bar-options-menu): Add checkbox for Adaptive Wrap
;; 	to the Line Wrapping submenu.
;; 	(adaptive-wrap-unload-function): New function.
;;
;; 2013-02-01  Stephen Berman  <stephen.berman@gmx.net>
;;
;; 	Fix error during redisplay: (wrong-type-argument stringp nil)
;;
;; 2012-12-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;;
;; 	* adaptive-wrap.el (adaptive-wrap-extra-indent): Fix buffer-localness.
;; 	Reported by Jonathan Kotta <jpkotta@gmail.com>.
;;
;; 2012-10-30  Stefan Monnier  <monnier@iro.umontreal.ca>
;;
;; 	Clean up copyright notices.
;;
;; 2012-05-21  Jonathan Kotta  <jpkotta@gmail.com>  (tiny change)
;;
;; 	Add adaptive-wrap-extra-indent.
;; 	* adaptive-wrap/adaptive-wrap.el (adaptive-wrap-extra-indent): New var.
;; 	(adaptive-wrap-fill-context-prefix): New function.
;; 	(adaptive-wrap-prefix-function): Use it.
;; 	(adaptive-wrap-prefix-mode): Add to visual-line custom group.
;;
;; 2012-01-05  Chong Yidong  <cyd@gnu.org>
;;
;; 	Rename adaptive-wrap-prefix to adaptive-wrap.
;;
;; 	The old name overflowed the column in list-packages.
;;
;; 2011-12-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;;
;; 	* adaptive-wrap-prefix.el: Improve commentary.
;;
;; 2011-12-04  Stefan Monnier  <monnier@iro.umontreal.ca>
;;
;; 	Rename awp-mode since we're not bound to 8+3.
;;
;; 2011-11-25  Stefan Monnier  <monnier@iro.umontreal.ca>
;;
;; 	* awp-mode: New package.
;;

(provide 'adaptive-wrap)
;;; adaptive-wrap.el ends here
