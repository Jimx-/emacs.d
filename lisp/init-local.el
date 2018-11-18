(setq user-full-name "Jin Xue")

(set-email-account
 "outlook"
 '((user-mail-address . "csjinxue@outlook.com")
   (mu4e-sent-folder . "/outlook/Sent")
   (mu4e-drafts-folder . "/outlook/Drafts")
   (mu4e-trash-folder . "/outlook/Deleted")
   (mu4e-refile-folder . "/outlook/Archive")
   (mu4e-compose-signature . "Jin Xue")
   (smtpmail-smtp-server . "smtp.office365.com")
   (smtpmail-smtp-service . 587)
   (smtpmail-smtp-user . "csjinxue@outlook.com"))
 t)

(provide 'init-local)
