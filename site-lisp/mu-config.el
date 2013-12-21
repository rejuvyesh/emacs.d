;;; Commentary:

;;; Code:
(require 'mu4e)
(require 'smtpmail)
(require 'org-mu4e)
;; (require 'mu4e-multi)

(setq mu4e-maildir "~/mail")
(setq mu4e-get-mail-command "offlineimap")
(setq message-kill-buffer-on-exit t)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; don't prompt for applying of marks, just apply
(setq mu4e-headers-leave-behavior 'apply)

;; Try to display images in mu4e
(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      mu4e-html2text-command "w3m -dump -T text/html")

;; Start mu4e in fullscreen, immediately ping for new mail
(defun mu4e-up-to-date-status ()
  (interactive)
  (window-configuration-to-register :mu4e-fullscreen)
  (mu4e)
  (mu4e-update-mail-show-window)
  (delete-other-windows))

;; Restore previous window configuration
(defun mu4e-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :mu4e-fullscreen))

(define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)
(define-key mu4e-headers-mode-map (kbd "M-u") 'mu4e-update-mail-show-window)

(setq mu4e-sent-folder "/mail/[Gmail].Sent Mail"
      mu4e-drafts-folder "/mail/[Gmail].Drafts"
      mu4e-trash-folder  "/mail/[Gmail].Trash"
      user-mail-address "mail@rejuvyesh.com"
      message-signature-file "~/.signature"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-stream-type starttls
      smtpmail-smtp-service 587)

;; define only variables that differ for the two accounts
(defvar my-mu4e-account-alist
  '(("main"
     (mu4e-drafts-folder "/mail/[Gmail].Drafts")
     (mu4e-sent-folder   "/mail/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/mail/[Gmail].Trash")
     (user-mail-address  "mail@rejuvyesh.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587))
    ("iitk"
     (mu4e-drafts-folder "/iitk/INBOX.Drafts")
     (mu4e-sent-folder   "/iitk/INBOX.Sent")
     (mu4e-trash-folder  "/iitk/INBOX.Trash")
     (user-mail-address  "jayeshkg@iitk.ac.in")
     (smtpmail-default-smtp-server "smtp.cc.iitk.ac.in")
     (smtpmail-smtp-server "smtp.cc.iitk.ac.in")
     (smtpmail-smtp-service 25))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


(provide 'mu-config)
;; (setq mu4e-multi-account-alist
;;       '(("main"
;;          (user-mail-address . "mail@rejuvyesh.com")
;;          (mu4e-drafts-folder . "/main/[Gmail].Drafts")
;;          (mu4e-follow-up-folder . "/main/FollowUp")
;;          (mu4e-hold-folder . "/main/Hold")
;;          (mu4e-refile-folder . "/main/[Gmail]Archived")
;;          (mu4e-sent-folder . "/personal/Sent")
;;          (mu4e-trash-folder . "/main/[Gmail].Trash"))
;;         ("work"
;;          (user-mail-address . "work@someotherserver.com")
;;          (mu4e-drafts-folder . "/work/Drafts")
;;          (mu4e-follow-up-folder . "/work/FollowUp")
;;          (mu4e-hold-folder . "/work/Hold")
;;          (mu4e-refile-folder . "/work/Archived")
;;          (mu4e-sent-folder . "/work/Sent")
;;          (mu4e-trash-folder . "/work/Trash"))))

;; (mu4e-multi-enable)

;; (define-key 'mu4e-headers-mode-map "h" 'mu4e-multi-mark-for-hold)
;; (define-key 'mu4e-headers-mode-map "f" 'mu4e-multi-mark-for-follow-up)

