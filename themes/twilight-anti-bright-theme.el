;;; twilight-anti-bright-theme.el --- based on twilight-anti-bright
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:

(deftheme twilight-anti-bright
  "A soothing light-on-dark theme.")

(let ((background "gray20")
      (foreground "#dcdddd")
      (selection "#313c4d")
      (hl-line "#1A1A1A")
      (cursor "#b4b4b4")
      (comment "#716d73")

      (gray-1 "#878289")   (gray-1bg "#181d23")
      (gray-2 "#2a3441")
      (gray-3 "#b3adb4")   (gray-3bg "#0e1116")
      (gray-4 "#1f2730")
      (gray-5 "#242d38")
      (gray-6 "#192028")
      (red-1 "#d15120")    (red-1bg "#2a1f1f")
      (red-2 "#b23f1e")    (red-2bg "#251c1e")
      (brown-1 "#9f621d")  (brown-1bg "#2a1f1f")
      (orange-1 "#d97a35") (orange-1bg "#272122")
      (yellow-1 "#deae3e") (yellow-1bg "#2a2921")
      (green-1 "#81af34")  (green-1bg "#1a2321")
      (green-2 "#4e9f75")  (green-2bg "#1a2321")
      (blue-1 "#7e9fc9")   (blue-1bg "#1e252f")
      (blue-2 "#417598")   (blue-2bg "#1b333e")
      (blue-3 "#00959e")   (blue-3bg "#132228")
      (blue-4 "#365e7a")   (blue-4bg "#172028")
      (purple-1 "#a878b5") (purple-1bg "#25222f")
      )

  (custom-theme-set-faces
   'twilight-anti-bright

   ;; Basics
   `(default ((t (:background ,background :foreground ,foreground))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,selection))))
   `(highlight ((default (:foreground ,blue-3))
                (((type graphic)) :background ,blue-3bg)))
   `(hl-line ((t (:background ,hl-line))))
   `(minibuffer-prompt ((default (:foreground ,orange-1))
                        (((type graphic)) :background ,orange-1bg)))
   `(escape-glyph ((default (:foreground ,purple-1))
                   (((type graphic)) :background , purple-1bg)))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((default (:foreground ,yellow-1))
                             (((type graphic)) :background ,yellow-1bg)))
   `(font-lock-constant-face ((default (:foreground ,purple-1))
                              (((type graphic)) :background ,purple-1bg)))
   `(font-lock-comment-face ((default (:foreground ,comment :italic t))
                             (((type graphic)) :background ,gray-1bg)))
   `(font-lock-doc-face ((default (:foreground ,gray-1))
                         (((type graphic)) :background ,gray-1bg)))

   `(font-lock-doc-string-face ((default (:foreground ,gray-1))
                                (((type graphic)) :background ,gray-1bg)))

   `(font-lock-function-name-face ((default (:foreground ,red-1))
                                   (((type graphic)) :background ,red-1bg)))
   `(font-lock-keyword-face ((default (:foreground ,orange-1))
                             (((type graphic)) :background ,orange-1bg)))
   `(font-lock-negation-char-face ((default (:foreground ,yellow-1))
                                   (((type graphic)) :background ,yellow-1bg)))
   `(font-lock-preprocessor-face ((default (:foreground ,orange-1))
                                  (((type graphic)) :background ,orange-1bg)))
   `(font-lock-string-face ((default (:foreground ,green-1))
                            (((type graphic)) :background ,green-1bg)))
   `(font-lock-type-face ((default (:foreground ,red-2 :bold nil))
                          (((type graphic)) :background ,red-2bg)))
   `(font-lock-variable-name-face ((default (:foreground ,blue-1))
                                   (((type graphic)) :background ,blue-1bg)))
   `(font-lock-warning-face ((default (:foreground ,red-2))
                             (((type graphic)) :background ,red-2bg)))

   ;; UI related
   `(link ((default (:foreground ,blue-1))
           (((type graphic)) :background ,blue-1bg)))
   `(fringe ((((type graphic)) :background ,gray-1bg)))
   `(mode-line ((default (:foreground ,blue-1))
                (((type graphic)) :background ,blue-2bg)))
   `(mode-line-inactive ((default (:foreground ,blue-4))
                         (((type graphic)) :background ,gray-4)))
   `(vertical-border ((default (:foreground ,gray-5))
                      (((type graphic)) :background ,background)))

   ;; Linum
   `(linum ((default (:foreground ,gray-2))
            (((type graphic)) :background ,gray-1bg)))

   ;; show-paren-mode
   `(show-paren-match ((default (:foreground ,orange-1))
                       (((type graphic)) :background ,orange-1bg)))
   `(show-paren-mismatch ((default (:foreground ,red-2bg))
                          (((type graphic)) :background ,red-2)))

   ;; ido
   `(ido-only-match ((default (:foreground ,green-1))
                     (((type graphic)) :background ,green-1bg)))
   `(ido-subdir ((default (:foreground ,purple-1))
                 (((type graphic)) :background ,purple-1bg)))

   ;; whitespace-mode
   `(whitespace-empty ((default (:foreground ,yellow-1bg))
                       (((type graphic)) :background ,yellow-1)))
   `(whitespace-hspace ((t (:foreground ,gray-2))))
   `(whitespace-indentation ((t (:foreground ,gray-2))))
   `(whitespace-line ((default ))
                     (((type graphic)) :background ,gray-2))
   `(whitespace-newline ((t (:foreground ,gray-2))))
   `(whitespace-space ((t (:foreground ,gray-2))))
   `(whitespace-space-after-tab ((t (:foreground ,gray-2))))
   `(whitespace-tab ((t (:foreground ,gray-2))))
   `(whitespace-trailing ((default (:foreground ,red-1bg))
                          (((type graphic)) :background ,red-1)))

   ;; flyspell-mode
   `(flyspell-incorrect ((t (:underline ,red-2))))
   `(flyspell-duplicate ((t (:underline ,red-2))))

   ;; magit
   `(magit-diff-add ((t (:foreground ,green-1))))
   `(magit-diff-del ((t (:foreground ,red-2))))
   `(magit-item-highlight ((((type graphic)) :background ,gray-1bg)))

   ;; highlight-indentation-mode
   `(highlight-indentation-face ((((type graphic)) (:background ,gray-1bg))))
   `(highlight-indentation-current-column-face ((((type graphic)) (:background ,gray-4))))

   ;; ECB
   `(ecb-default-general-face ((default (:foreground ,gray-3))
                               (((type graphic)) :background ,gray-1bg)))
   `(ecb-default-highlight-face ((default (:foreground ,red-1))
                                 (((type graphic)) :background ,red-1bg)))
   `(ecb-method-face ((default (:foreground ,red-1))
                      (((type graphic)) :background ,red-1bg)))
   `(ecb-tag-header-face ((((type graphic)) (:background ,blue-2bg))))

   ;; org-mode
   `(org-date ((default (:foreground ,purple-1))
               (((type graphic)) :background ,purple-1bg)))
   `(org-done ((default (:foreground ,green-1))
               (((type graphic)) :background ,green-1bg)))
   `(org-hide ((default (:foreground ,gray-2))
               (((type graphic)) :background ,gray-1bg)))
   `(org-link ((default (:foreground ,blue-1))
               (((type graphic)) :background ,blue-1bg)))
   `(org-todo ((default (:foreground ,red-1))
               (((type graphic)) :background ,red-1bg)))
   )

  (custom-theme-set-variables
   'twilight-anti-bright

   ;; ;; Fill Column Indicator mode
   `(fci-rule-color ,gray-6)
   `(fci-rule-character-color ,gray-6)

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-1 ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-1 ,foreground])
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'twilight-anti-bright)

;;; twilight-anti-bright-theme.el ends here
