;; helm
(use-package helm
  :ensure t
  :defer t
  :bind
  (("C-c h"     . helm-command-prefix)
   ("C-c h t"   . helm-cmd-t)
   ("C-c h g"   . helm-do-grep-ag)
   ("C-c h o"   . helm-occur)
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring)
   ("C-c h C-o" . helm-swoop)
   ("C-x C-f"   . helm-find-files)
   ("C-x b"     . helm-mini))
  :init
  (helm-mode t)
  :config
  (require 'helm-config)  
  (setq helm-candidate-number-limit 200)
  (setq helm-idle-delay 0.01
        helm-exit-idle-delay 0.1
        helm-input-idle-delay 0.01
        helm-yas-display-key-on-candidate t
        helm-quick-update t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t
        helm-split-window-default-side 'right
        helm-ff-lynx-style-map nil
        helm-follow-mode-persistent t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-completion-in-region-fuzzy-match t)
  (setq enable-recursive-minibuffers t)
   ;; Too close to exit
  (global-unset-key (kbd "C-x c"))
  (defadvice helm-default-display-buffer
      (before helm-fullscreen-split activate)
    (delete-other-windows))
   )

(provide 'setup-helm)
