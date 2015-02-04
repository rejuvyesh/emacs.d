;; mail settings

(setup-lazy '(mu mu4e) "mu4e"
  (setq mu4e-maildir "~/mail/iitk")
  
  (setq mu4e-drafts-folder "/INBOX.Drafts")
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")
  
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  
  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to

  (setq mu4e-maildir-shortcuts
        '(("/inbox" . ?i)
          ("/sent"	. ?s)
          ("/trash" . ?t)
          ))
  (setq mu4e-bookmarks
        `((,(concat
             "flag:unread"
             " AND NOT flag:trashed" ) "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images"	?p)))
  
  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "sandesh")
  
  ;; saving stuff
  (setq mu4e-attachment-dir (expand-file-name "~/stuff"))
  
  ;; no confirmations
  (setq mu4e-confirm-quit nil)
  
  (setup "mu4e-contrib")
  (setq mu4e-html2text-command 'mu4e-shr2text)
  
  ;; Try to display images in mu4e
  (setq
   mu4e-view-show-images t
   mu4e-view-image-max-width 800)
  
  
  )

(defalias 'mu 'mu4e)

(setup-after "mu4e" "smtpmail")
;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Jayesh Kumar Gupta")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'setup-mail)
