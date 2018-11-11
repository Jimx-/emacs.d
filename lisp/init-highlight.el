;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching parentheses
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight indentions
(when (display-graphic-p)
  (use-package highlight-indent-guides
    :diminish
    :hook (prog-mode . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-responsive t)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :hook ((emacs-lisp-mode web-mode css-mode) . rainbow-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

;; Flash the current line
(use-package nav-flash
  :defines compilation-highlight-overlay
  :functions windmove-do-window-select
  :preface
  (defun my-blink-cursor-maybe (orig-fn &rest args)
    "Blink current line if the window has moved."
    (ignore-errors
      (let ((point (save-excursion (goto-char (window-start))
                                   (point-marker))))
        (apply orig-fn args)
        (unless (or (derived-mode-p 'term-mode)
                    (equal point
                           (save-excursion (goto-char (window-start))
                                           (point-marker))))
          (my-blink-cursor)))))

  (defun my-blink-cursor (&rest _)
    "Blink current line using `nav-flash'."
    (interactive)
    (unless (minibufferp)
      (nav-flash-show)
      ;; only show in the current window
      (overlay-put compilation-highlight-overlay 'window (selected-window))))
  :hook ((imenu-after-jump
          switch-window-finish
          counsel-grep-post-action
          dumb-jump-after-jump)
         . my-blink-cursor)
  :init
  ;; NOTE In :feature jump `recenter' is hooked to a bunch of jumping commands,
  ;; which will trigger nav-flash.
  (advice-add #'windmove-do-window-select :around #'my-blink-cursor-maybe)
  (advice-add #'other-window :around #'my-blink-cursor-maybe)
  (advice-add #'recenter :around #'my-blink-cursor-maybe))

(provide 'init-highlight)
