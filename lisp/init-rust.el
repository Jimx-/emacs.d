(use-package rustic
  :if (> emacs-major-version 26)
  :init
  (setq rustic-rls-pkg 'lsp-mode)
  :config
  (setq rustic-indent-method-chain t
        rustic-flycheck-setup-mode-line-p nil))

(use-package cargo
  :disabled
  :hook ((rust-mode rustic-mode) . cargo-minor-mode)
  :config
  (setq cargo-process--enable-rust-backtrace t))

(provide 'init-rust)
