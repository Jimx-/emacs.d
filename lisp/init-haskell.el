(use-package haskell-mode
  :config
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  ;; (use-package intero
  ;;   :commands intero-global-mode
  ;;   :hook (haskell-mode . intero-global-mode))
  )

(provide 'init-haskell)
