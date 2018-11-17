;; Offer a *visual* way to choose a window to switch to
(use-package switch-window
  :bind (("C-x o" . switch-window))
  :config
  (setq switch-window-shortcut-style 'alphabet
        switch-window-timeout nil))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(provide 'init-windows)
