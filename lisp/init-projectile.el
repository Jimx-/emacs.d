(use-package projectile
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val))

  ;; Rails project
  (use-package projectile-rails
    :diminish projectile-rails-mode
    :hook (projectile-mode . projectile-rails-global-mode)))

(provide 'init-projectile)
