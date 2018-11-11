(use-package exec-path-from-shell
  :config
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (memq window-system '(mac ns x))
  (setq-default exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
