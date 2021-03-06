(use-package projectile
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-find-file)
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)   ; Update mode-line at the first time

  (setq projectile-git-submodule-command nil)

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

(provide 'init-projectile)
