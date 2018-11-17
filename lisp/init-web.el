(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; Major mode for editing web templates
(use-package web-mode
  :defines company-backends
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web)
