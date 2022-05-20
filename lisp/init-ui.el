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

;; Optimizations
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Modeline

(use-package nyan-mode
  :init (setq nyan-animate-nyancat t
              nyan-bar-length 16
              nyan-wavy-trail t)
  :hook ((after-init . nyan-mode)))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-init)
         (dashboard-mode . doom-modeline-set-project-modeline))
  :init
  (setq doom-modeline-icon (display-graphic-p)
        doom-modeline-minor-modes t)
  (unless after-init-time
    (setq-default mode-line-format nil)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           pdf-annot-list-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (with-no-warnings
    (defun all-the-icons-reset ()
      "Reset the icons."
      (interactive)
      (dolist (func '(all-the-icons-icon-for-dir
                      all-the-icons-icon-for-file
                      all-the-icons-icon-for-mode
                      all-the-icons-icon-for-url
                      all-the-icons-icon-family-for-file
                      all-the-icons-icon-family-for-mode
                      all-the-icons-icon-family))
        (all-the-icons-cache func))
      (message "Reset all-the-icons")))

  ;; Support more icons
  (let ((extension-icon-alist
         '(("bat"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("cmd"  all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver)
           ("conf" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("eln"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("epub" all-the-icons-faicon "book"         :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           ("exe"  all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-dsilver)
           ("make" all-the-icons-fileicon "gnu"        :face all-the-icons-dorange)
           ("rss"  all-the-icons-octicon "rss"         :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           ("toml" all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-yellow)
           ("tsx"  all-the-icons-fileicon "tsx"        :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)
           ("xpm"  all-the-icons-octicon "file-media"  :v-adjust 0.0 :face all-the-icons-dgreen))))
    (dolist (icon extension-icon-alist)
      (add-to-list 'all-the-icons-extension-icon-alist icon)))

  (let ((regexp-icon-alist
         '(("\\.[bB][iI][nN]$"               all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow)
           ("^config$"                       all-the-icons-octicon "settings"    :v-adjust 0.0 :face all-the-icons-dorange)
           ("\\.\\(ba\\|z\\)shrc$"           all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dpink)
           ("\\.\\(bash\\|zsh\\)*_?profile$" all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("\\.\\(ba\\|z\\)sh_history$"     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dsilver)
           ("\\.zshenv$"                     all-the-icons-alltheicon "script"   :height 0.9 :face all-the-icons-dred)
           ("Cask\\'"                        all-the-icons-fileicon "elisp"      :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           ("NEWS$"                          all-the-icons-faicon "newspaper-o"  :height 0.9 :v-adjust -0.2)
           ("^Rakefile$"                     all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red)
           ("^go.\\(sum\\|mod\\)$"           all-the-icons-fileicon "go"         :face all-the-icons-dpurple))))
    (dolist (icon regexp-icon-alist)
      (add-to-list 'all-the-icons-regexp-icon-alist icon)))

  (let ((mode-icon-alist
         '((xwidget-webkit-mode           all-the-icons-faicon "chrome"          :v-adjust -0.1 :face all-the-icons-blue)
           (bongo-playlist-mode           all-the-icons-material "queue_music"   :height 1.2 :face all-the-icons-green)
           (bongo-library-mode            all-the-icons-material "library_music" :height 1.1 :face all-the-icons-green)
           (gnus-group-mode               all-the-icons-fileicon "gnu"           :face all-the-icons-silver)
           (gnus-summary-mode             all-the-icons-octicon "inbox"          :height 1.0 :v-adjust 0.0 :face all-the-icons-orange)
           (gnus-article-mode             all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (message-mode                  all-the-icons-octicon "mail"           :height 1.1 :v-adjust 0.0 :face all-the-icons-lblue)
           (diff-mode                     all-the-icons-octicon "git-compare"    :v-adjust 0.0 :face all-the-icons-lred)
           (flycheck-error-list-mode      all-the-icons-octicon "checklist"      :height 1.1 :v-adjust 0.0 :face all-the-icons-lred)
           (elfeed-search-mode            all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (elfeed-show-mode              all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (newsticker-mode               all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-mode      all-the-icons-faicon "rss-square"      :v-adjust -0.1 :face all-the-icons-orange)
           (newsticker-treeview-list-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-orange)
           (newsticker-treeview-item-mode all-the-icons-octicon "rss"            :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange)
           (conf-mode                     all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (conf-space-mode               all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-yellow)
           (gitconfig-mode                all-the-icons-octicon "settings"       :v-adjust 0.0 :face all-the-icons-dorange)
           (forge-topic-mode              all-the-icons-alltheicon "git"         :face all-the-icons-blue)
           (help-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (helpful-mode                  all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1 :face all-the-icons-purple)
           (Info-mode                     all-the-icons-faicon "info-circle"     :height 1.1 :v-adjust -0.1)
           (cask-mode                     all-the-icons-fileicon "elisp"         :height 1.0 :v-adjust -0.2 :face all-the-icons-blue)
           (ein:notebooklist-mode         all-the-icons-faicon "book"            :face all-the-icons-lorange)
           (ein:notebook-mode             all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-orange)
           (ein:notebook-multilang-mode   all-the-icons-fileicon "jupyter"       :height 1.2 :face all-the-icons-dorange)
           (nov-mode                      all-the-icons-faicon "book"            :height 1.0 :v-adjust -0.1 :face all-the-icons-green)
           (gfm-mode                      all-the-icons-octicon "markdown"       :face all-the-icons-lblue)
           (osx-dictionary-mode           all-the-icons-material "library_books" :face all-the-icons-lblue)
           (youdao-dictionary-mode        all-the-icons-material "library_books" :face all-the-icons-lblue)
           (fanyi-mode                    all-the-icons-material "library_books" :face all-the-icons-lblue))))
    (dolist (icon mode-icon-alist)
      (add-to-list 'all-the-icons-mode-icon-alist icon))))

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

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Good pixel line scrolling
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (when (> emacs-major-version 27)
    (use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))))

;; Miscs

(unless (> emacs-major-version 26) ; moved to early-init.el in emacs 27
  (tooltip-mode -1)              ; relegate tooltips to echo area only
  (menu-bar-mode -1)
  (if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-ui)
