(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-search-filter "@2-week-ago"))

(use-package elfeed-org
  :preface
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (and (let ((default-directory org-directory))
         (setq rmh-elfeed-org-files
               (cl-remove-if-not
                #'file-exists-p (mapcar #'expand-file-name rmh-elfeed-org-files))))
       (elfeed-org)))

(provide 'init-rss)
