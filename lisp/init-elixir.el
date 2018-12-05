(use-package elixir-mode
  :defer t)

(use-package alchemist
  :hook (elixir-mode . alchemist-mode))

(provide 'init-elixir)
