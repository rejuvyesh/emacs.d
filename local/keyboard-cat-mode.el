;;; keyboard-cat-mode.el ---

;;; Commentary:

;; Typing for keyboard cats. Based off zk_phi's code.

;;; Code:

(defvar keyboard-cat-overlay nil)

(defvar keyboard-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'keyboard-cat-next)
    (define-key map [remap keyboard-quit] 'keyboard-cat-mode)
    map))

(define-minor-mode keyboard-cat-mode
  "keyboard cat mode"
  :init nil
  :keymap keyboard-cat-mode-map
  (if keyboard-cat-mode
      (overlay-put
       (setq keyboard-cat-overlay
             (make-overlay (point-min) (point-max)))
       'invisible t)
    (delete-overlay keyboard-cat-overlay)))

(defun keyboard-cat-next ()
  (interactive)
  (move-overlay keyboard-cat-overlay
                (goto-char (min (+ (overlay-start keyboard-cat-overlay)
                                   (random 5)
                                   1)
                                (point-max)))
                (overlay-end keyboard-cat-overlay)))

(provide 'keyboard-cat-mode)
;;; keyboard-cat-mode.el ends here
