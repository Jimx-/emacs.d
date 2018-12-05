;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :bind (("S-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-k" . crux-smart-kill-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-file-and-buffer)
         ("s-j" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)))

(provide 'init-utils)
