;; If you don't customize it, this is the theme you get.
(defvar custom-enabled-theme 'leuven
  "A symbol representing the enabled theme.")

(defvar custom-font nil
  "The default font.")

(defvar custom-unicode-font nil
  "Fallback font for unicode glyph.")

(defvar custom-variable-pitch-font nil
  "The default font for variable-pitch text.")

;; Modeline

(use-package spaceline-config
  :ensure spaceline
  :defines (powerline-default-separator
            powerline-image-apple-rgb
            spaceline-pre-hook
            spaceline-highlight-face-func)
  :functions powerline-reset
  :hook (after-init . spaceline-spacemacs-theme)
  :init
  (setq powerline-default-separator (or (and (display-graphic-p) 'wave) 'utf-8))
  :config
  (setq spaceline-pre-hook #'powerline-reset) ; For changing themes
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

;; Theme

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name theme)))

(cond
 ((is-doom-theme-p custom-enabled-theme)
  (use-package doom-themes
    :init
    (load-theme custom-enabled-theme t)
    :config
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)

    ;; Enable custom treemacs theme (all-the-icons must be installed!)
    (doom-themes-treemacs-config)))

 (t
  (ignore-errors (load-theme custom-enabled-theme t))))

;; Fonts

(when (display-graphic-p)
  (when (fontp custom-font)
    (set-frame-font custom-font nil t)
    (set-face-attribute 'fixed-pitch nil :font custom-font))
  ;; Fallback to `custom-unicode-font' for Unicode characters
  (when (fontp custom-unicode-font)
    (set-fontset-font t 'unicode custom-unicode-font nil))
  ;; ...and for variable-pitch-mode:
  (when (fontp custom-variable-pitch-font)
    (set-face-attribute 'variable-pitch nil :font custom-variable-pitch-font)))

;; Miscs

(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-ui)
