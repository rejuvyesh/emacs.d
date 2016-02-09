;; misc helper functions

;; common lisp stuff
(use-package cl-lib
             :ensure t)

;; better convenience functions for strings and map/reduce/loop
(use-package dash
             :ensure t)
(use-package s
             :ensure t)

;; sane regexes
(use-package rx
             :ensure t)
(use-package ample-regexps
             :ensure t)

;; replace obsolete flet
(use-package noflet
             :ensure t)

;; For time based theme setup
(use-package solar)

;; some generic aliases that make elisp less painful
(defalias 'first  'cl-first)
(defalias 'head   'cl-first)
(defalias 'hd     'cl-first)
(defalias 'second 'cl-second)
(defalias 'third  'cl-third)
(defalias 'ninth  'cl-ninth)
(defalias 'tail   'cl-rest)
(defalias 'tl     'cl-rest)
(defalias 'fn     'lambda)
(defalias 'rest   'cl-rest)
(defalias 'loop   'cl-loop)
(defalias 'case   'cl-case)

(defun read-lines (filename)
  "Return a list of lines of a file at FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string) "\n" t)))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun region-as-string ()
  "Return region as string."
  (buffer-substring (region-beginning)
                    (region-end)))

(defun word-or-region ()
  "Returns word boundary or selected region."
  (let (beg
        end
        (deactivate-mark nil)
        (case-fold-search	nil))
    (if (region-active-p)
        (setq beg (region-beginning)
              end (region-end))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (setq beg (first bounds)
              end (rest bounds))))
    (list beg end)))

(defun pretty-load? ()
  "load stuff like themes that are only meaningful in window system?"
  (or (display-graphic-p)
      (daemonp)))

;; For time stuff
;; Stealing from (https://github.com/hadronzoo/theme-changer/)
(defun hour-fraction-to-time (date hour-fraction)
  (let*
      ((now (decode-time (current-time)))
       
       (month (first   date))
       (day   (second  date))
       (year  (third   date))
       (zone  (ninth   now))
       
       (frac-hour (cl-truncate hour-fraction))
       (hour (first frac-hour))

       (frac-minutes (cl-truncate (* (second frac-hour) 60)))
       (minute (first frac-minutes))

       (frac-seconds (cl-truncate (* (second frac-minutes) 60)))
       (sec (first frac-seconds)))
    (encode-time sec minute hour day month year zone)))


(defun sunrise-sunset-times (date)
  (let*
      ((l (solar-sunrise-sunset date))
       (sunrise-time (hour-fraction-to-time date (caar l)))
       (sunset-time (hour-fraction-to-time date (cl-caadr l))))
    (list sunrise-time sunset-time)))

(defun daytime-p (sunrise-time sunset-time)
  (let* ((now (current-time)))
    (and (time-less-p sunrise-time now)
	 (time-less-p now sunset-time))))

(defun today () (calendar-current-date))

(defun tomorrow ()
  (calendar-gregorian-from-absolute
   (+ 1 (calendar-absolute-from-gregorian (today)))))

(defun +second (time)
  (time-add time (seconds-to-time 1)))

;; Set location
(setq calendar-latitude 37.4)
(setq calendar-longitude -122.2)
(setq calendar-location-name "Stanford, CA")

(provide 'setup-helpers)
