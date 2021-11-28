(require 'org-mu4e)
(add-hook 'org-mode-hook 'mu4e-dashboard-mode)

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/.mail")

;; Set up contexts for email accounts
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "qq"
          :match-func (lambda (msg) (when msg
                                      (string-prefix-p "/qq" (mu4e-message-field msg :maildir))))
          :vars '(
                  (user-full-name . "Xing Wenju")
                  (user-mail-address . "linuxing3@qq.com")
                  (mu4e-sent-folder . "/qq/Sent")
                  (mu4e-trash-folder . "/qq/Trash")
                  (mu4e-drafts-folder . "/qq/Drafts")
                  (mu4e-refile-folder . "/qq/Archive")
                  (mu4e-sent-messages-behavior . sent)
                  ))
        ,(make-mu4e-context
          :name "gmail"
          :match-func (lambda (msg) (when msg
                                      (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
          :vars '(
                  (mu4e-sent-folder . "/gmail/Sent")
                  (mu4e-trash-folder . "/gmail/Deleted")
                  (mu4e-refile-folder . "/gmail/Archive")
                  ))
        ))
(setq mu4e-context-policy 'pick-first)

;; Display options
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)

;; Composing mail
(setq mu4e-compose-dont-reply-to-self t)

;; Use mu4e for sending e-mail
(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "xingwenju"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl)

;; Signing messages (use mml-secure-sign-pgpmime)
(setq mml-secure-openpgp-signers '("0D955887B42DEFC8B496E38C9AA4F44E6183E2B1"))

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.
(setq mu4e-maildir-shortcuts
      '(("/qq/Inbox"       . ?i)
        ("/qq/Lists/*"     . ?l)
        ("/qq/Sent Mail"   . ?s)
        ("/qq/Trash"       . ?t)))

(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "All Inboxes"
              :query "maildir:/qq/Inbox OR maildir:/gmail/Inbox"
              :key ?i))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq dw/mu4e-inbox-query
      "(maildir:/gmail/Inbox OR maildir:/qq/Inbox) AND flag:unread")

(defun dw/go-to-inbox ()
  (interactive)
  (mu4e-headers-search dw/mu4e-inbox-query))

;; Start mu4e in the background so that it syncs mail periodically
(mu4e t)

(provide 'init-mail)
