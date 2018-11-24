;; If you don't customize it, this is the theme you get.
(defvar custom-enabled-theme 'leuven
  "A symbol representing the enabled theme.")

(defvar custom-font nil
  "The default font.")

(defvar custom-unicode-font nil
  "Fallback font for unicode glyph.")

(defvar custom-variable-pitch-font nil
  "The default font for variable-pitch text.")

(setq
 frame-title-format
 '("emacs@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b")))
 icon-title-format frame-title-format)

 ;; Modeline

(use-package smart-mode-line
  :disabled
  :ensure t
  :hook (after-init . sml/setup)
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/projects/\\(\\w+\\)/"
                 (lambda (s) (concat ":Prj<" (match-string 1 s) ">:")
                   ))
               t))

(use-package spaceline-config
  :disabled
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

(use-package doom-modeline
  :init (setq doom-modeline-icon (display-graphic-p))
  :hook ((after-init . doom-modeline-init)
         (dashboard-mode . doom-modeline-set-project-modeline)))

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

;; Line number
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :init
      (setq linum-highlight-in-all-buffersp t)
      (custom-set-faces
       `(linum-highlight-face
         ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))))

;; Miscs

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

(unless (> emacs-major-version 26)     ; moved to early-init.el in emacs 27
  (tooltip-mode -1)              ; relegate tooltips to echo area only
  (menu-bar-mode -1)
  (if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-ui)
