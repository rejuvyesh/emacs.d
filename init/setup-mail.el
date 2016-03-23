;; mail settings

(defalias 'mu 'mu4e)
(use-package mu4e
  :commands (mu4e mu4e-compose-new)
  :config
;;; Set up some common mu4e variables
  (setq mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        mu4e-sent-folder "/sent"
        mu4e-get-mail-command "sandesh"
        mu4e-update-interval nil
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-headers-replied-mark '("R" . "↵")
        mu4e-headers-encrypted-mark '("x" . "⚷"))
  ;; saving stuff
  (setq mu4e-attachment-dir (expand-file-name "~/stuff"))
  (setq mu4e-completing-read-function 'helm--completing-read-default)
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)
  
  ;; html
  (use-package mu4e-contrib
    :config
    (setq mu4e-html2text-command 'mu4e-shr2text))
  
  ;; Try to display images in mu4e
  (setq
   mu4e-view-show-images t
   mu4e-view-image-max-width 800)
  
  ;; no confirmations
  (setq mu4e-confirm-quit nil)
  
  ;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)))  
  ;; Contexts
  (setq mu4e-contexts
        `(,(make-mu4e-context
             :name "stanford"
             :enter-func (lambda () (mu4e-message "Switched to the Stanford"))
             ;; leave-fun not defined
             :match-func (lambda (msg)
                           (when msg 
                             (mu4e-message-contact-field-matches msg 
                                                                 :to "stanford.edu")))
             :vars '(  (user-mail-address  . "jkg@cs.stanford.edu")
                       (user-full-name     . "Jayesh K. Gupta")
                       (mu4e-maildir       . "~/mail/stanford")
                       (mu4e-compose-signature .
                                               (concat
                                                "Jayesh K. Gupta\n"
                                                "http://cs.stanford.edu/~jkg\n"))))
          ,(make-mu4e-context
             :name "rejuvyesh"
             :enter-func (lambda () (mu4e-message "Switched to the mail@rejuvyesh"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg 
                             (mu4e-message-contact-field-matches msg 
                                                                 :to (list "mail@rejuvyesh.com" 
                                                                           "a2z.jayesh@gmail.com"))))
             :vars '(  (user-mail-address . "mail@rejuvyesh.com")
                       (user-full-name    . "Jayesh K Gupta")
                       (mu4e-maildir       . "~/mail/rejuvyesh")
                       ( mu4e-compose-signature .
                                                (concat
                                                 "Jayesh K. Gupta\n"
                                                 "http://rejuvyesh.com\n"))))
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda () (mu4e-message "Switched to the rejuvyesh@gmail"))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (when msg 
                            (mu4e-message-contact-field-matches msg 
                                                                :to (list "rejuvyesh@gmail.com"))))
            :vars '(  (user-mail-address . "rejuvyesh@gmail.com")
                      (user-full-name    . "rejuvyesh")
                      (mu4e-maildir       . "~/mail/gmail")
                      ( mu4e-compose-signature .
                                               (concat
                                                "rejuvyesh\n"
                                                "http://rejuvyesh.com\n"))))
          ))
  (use-package org-mu4e
    :defer t)
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  )

;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Jayesh Kumar Gupta")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'setup-mail)
