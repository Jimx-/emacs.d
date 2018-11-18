;; -*- lexical-binding: t -*-

(defun set-email-account (label letvars &optional default-p)
  (with-eval-after-load 'mu4e
    (when-let* ((address (cdr (assq 'user-mail-address letvars))))
      (add-to-list 'mu4e-user-mail-address-list address))
    (setq mu4e-contexts
          (cl-loop for context in mu4e-contexts
                   unless (string= (mu4e-context-name context) label)
                   collect context))
    (let ((context (make-mu4e-context
                    :name label
                    :enter-func (lambda () (mu4e-message "Switched to %s" label))
                    :leave-func #'mu4e-clear-caches
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir))))
                    :vars letvars)))
      (push context mu4e-contexts)
      (when default-p
        (setq-default mu4e-context-current context))
      context)))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands (mu4e mu4e-compose-new)
  :init
  (provide 'html2text)
  (setq mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/.mail/.attachments"
        mu4e-user-mail-address-list nil)
  :config
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-change-filenames-when-moving t
        mu4e-update-interval nil
        mu4e-compose-format-flowed t
        mu4e-view-show-addresses t
        mu4e-sent-messages-behavior 'sent
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t   ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        mu4e-completing-read-function #'ivy-completing-read
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))


  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  (use-package mu4e-maildirs-extension
    :config
    (mu4e-maildirs-extension)
    (setq mu4e-maildirs-extension-title nil
          ;; mu4e-maildirs-extension-ignored-regex "^*~*"
          mu4e-maildirs-extension-action-text "\t[g] Update mail and index\n"
          mu4e-maildirs-extension-maildir-expanded-prefix "-"
          mu4e-maildirs-extension-maildir-default-prefix "|"))


  (use-package org-mu4e
    :ensure nil
    :hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
    :config
    (setq org-mu4e-link-query-in-headers-mode nil
          org-mu4e-convert-to-html t)

    ;; Only render to html once. If the first send fails for whatever reason,
    ;; org-mu4e would do so each time you try again.
    (add-hook 'message-send-hook
              (lambda () (setq-local org-mu4e-convert-to-html nil)))))

(provide 'init-mail)
