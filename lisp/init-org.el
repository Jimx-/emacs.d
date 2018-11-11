(use-package org
  :ensure nil
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)"))
        org-startup-indented t)

  ;; Fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t))

(provide 'init-org)
