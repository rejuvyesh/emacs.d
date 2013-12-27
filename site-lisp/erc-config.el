
;;; Code:

;; Requirements

(require 'erc)

(erc-autojoin-mode t)

(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#lesswrong" "#archlinux" "#emacs" "#ruby" )))

(add-hook 'erc-mode-hook 'erc-fill-disable)
(setq erc-max-buffer-size 10000)
(erc-truncate-mode 1)
(erc-scrolltobottom-mode 1)

;; Set logging location
(setq erc-log-channels-directory "~/.erc/logs/")

;; Save logs on exit or quit
(setq erc-save-buffer-on-part t)

;; Save all erc buffers on exit
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

(require 'erc-hl-nicks)

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(custom-set-faces
 '(erc-my-nick-face ((t (:foreground "#dca3a3" :weight bold)))))

(provide 'erc-config)
;;; erc-config.el ends here
