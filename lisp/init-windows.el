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
;; Navigate windows and frames using numbers
(use-package winum
  :bind (:map winum-keymap
              ("C-`" . winum-select-window-by-number)
              ("C-²" . winum-select-window-by-number)
              ("M-0" . winum-select-window-0-or-10)
              ("M-1" . winum-select-window-1)
              ("M-2" . winum-select-window-2)
              ("M-3" . winum-select-window-3)
              ("M-4" . winum-select-window-4)
              ("M-5" . winum-select-window-5)
              ("M-6" . winum-select-window-6)
              ("M-7" . winum-select-window-7)
              ("M-8" . winum-select-window-8)
              ("M-9" . winum-select-window-9))
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  (add-to-list 'winum-assign-functions
               (lambda ()
                 (cond
                  ((equal (buffer-name) " *Treemacs-Framebuffer-1*") 9)
                  ((equal (buffer-name) "*Flycheck errors*") 8)))))

(provide 'init-windows)
