
;; Offer a *visual* way to choose a window to switch to 
(use-package switch-window
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-shortcut-style 'alphabet
        switch-window-timeout nil))

(provide 'init-windows)
