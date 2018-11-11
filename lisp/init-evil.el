;; -*- lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-visual-char-semi-exclusive t
      evil-magic t
      evil-echo-state t
      evil-indent-convert-tabs t
      evil-ex-search-vim-style-regexp t
      evil-ex-substitute-global t
      evil-ex-visual-char-range t  ; column range for ex commands
      evil-insert-skip-empty-lines t
      evil-mode-line-format 'nil
      evil-respect-visual-line-mode t
      ;; more vim-like behavior
      evil-symbol-word-search t
      ;; don't activate mark on shift-click
      shift-select-mode nil
      ;; cursor appearance
      evil-default-cursor '+evil-default-cursor
      evil-normal-state-cursor 'box
      evil-emacs-state-cursor  '(box +evil-emacs-cursor)
      evil-insert-state-cursor 'bar
      evil-visual-state-cursor 'hollow
      ;; must be set before evil/evil-collection is loaded
      evil-want-keybinding t))

(provide 'init-evil)

