;;; keyboard-cat-mode.el ---

;;; Commentary:

;; Typing for keyboard cats. Based off zk_phi's code.

;;; Code:

(defvar keyboard-cat-overlay nil
  "Invisibility overlay for `keyboard-cat-mode'.")

(defvar keyboard-cat-step-function (lambda () 1)
  "Function that the number of characters to \"emit\" at a time.
This is a function so that a random number could be returned for
each individual key event.")

(defvar keyboard-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'keyboard-cat-next)
    (define-key map [remap keyboard-quit] 'keyboard-cat-mode)
    map)
  "Keymap for `keyboard-cat-mode'.")

(define-minor-mode keyboard-cat-mode
  "keyboard cat mode: Clear the buffer and set the keymap to re-type the buffer on all input. Gives the illusion that you're a fast typist."
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
