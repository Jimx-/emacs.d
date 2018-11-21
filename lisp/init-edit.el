(prefer-coding-system 'utf-8)

;; Basic preferences
(setq make-backup-files nil
      delete-by-moving-to-trash t)

;; Tab and Space
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; Delete selection if you insert
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))

;; Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Hideshow
(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode)

;; Editor config
(use-package editorconfig
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

(provide 'init-edit)
