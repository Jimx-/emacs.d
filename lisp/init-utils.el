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

(use-package rime
  :quelpa (rime :fetcher github
                :repo "DogLooksGood/emacs-rime"
                :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/.config/fcitx/rime"))

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
         ("c" . rg-dwim-current-dir)
         ("f" . rg-dwim-current-file)
         ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t))

(provide 'init-utils)
