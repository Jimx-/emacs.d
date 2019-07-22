(use-package ispell
  :ensure nil
  :config
  (add-to-list 'ispell-extra-args "--dont-tex-check-comments")

  (setq ispell-program-name "aspell"
        ispell-dictionary "english"))

(use-package flyspell-correct-ivy
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

(provide 'init-flyspell)
