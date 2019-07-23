(use-package rust-mode
  :config
  (setq rust-indent-method-chain t)

  ;; This is necessary because both plugins are fighting for supremacy in
  ;; `auto-mode-alist', so rustic-mode *must* load second. It only needs to
  ;; happen once.
  ;;
  ;; rust-mode is still required for `racer'.
  (defun +rust|init ()
    "Switch to `rustic-mode', if it's available."
    (when (require 'rustic nil t)
      (rustic-mode)))
  (add-hook 'rust-mode-hook #'+rust|init))

(use-package rustic
  :if (> emacs-major-version 26)
  :after rust-mode
  :init
  (setq rustic-rls-pkg 'lsp-mode)
  :config
  (setq rustic-indent-method-chain t
        rustic-flycheck-setup-mode-line-p nil))

(use-package cargo
  :after rust-mode
  :hook ((rust-mode rustic-mode) . cargo-minor-mode))

(provide 'init-rust)
