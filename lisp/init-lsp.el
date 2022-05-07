(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook (((go-mode ruby-mode php-mode
                   html-mode web-mode json-mode
                   css-mode less-mode sass-mode scss-mode
                   js-mode js2-mode typescript-mode
                   rust-mode groovy-mode) . lsp)
         (((haskell-mode rust-mode) . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer)))))
  :init
  (setq lsp-prefer-flymake nil
        lsp-signature-auto-activate nil
        lsp-lens-enable nil)

  (setq lsp-enable-file-watchers nil)

  ;; Support LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    ;; (cl-check-type lang symbolp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((lsp-file (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (lsp)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

(use-package lsp-ui
  :after lsp-mode
  ;; :load-path "~/projects/lsp-ui/"
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  ;; (setq lsp-ui-doc-use-webkit t)
  ;; (setq-default lsp-ui-doc-frame-parameters
  ;;               '((left . -1)
  ;;                 (top . -1)
  ;;                 (no-accept-focus . t)
  ;;                 (min-width . 0)
  ;;                 (width . 0)
  ;;                 (min-height . 0)
  ;;                 (height . 0)
  ;;                 (internal-border-width . 0)
  ;;                 (vertical-scroll-bars)
  ;;                 (horizontal-scroll-bars)
  ;;                 (left-fringe . 0)
  ;;                 (right-fringe . 0)
  ;;                 (menu-bar-lines . 0)
  ;;                 (tool-bar-lines . 0)
  ;;                 (line-spacing . 0.1)
  ;;                 (unsplittable . t)
  ;;                 (undecorated . t)
  ;;                 (minibuffer . nil)
  ;;                 (visibility . nil)
  ;;                 (mouse-wheel-frame . nil)
  ;;                 (no-other-frame . t)
  ;;                 (cursor-type)
  ;;                 (no-special-glyphs . t)))
  :config
  (setq lsp-ui-doc-enable t
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'top
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil))


;; C/C++/Objective-C lang server support for lsp-mode using clang
;; Install: yaourt ccls
;;          refer to  https://github.com/MaskRay/ccls/wiki/Getting-started
(defun clang-format-buffer-smart ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (and (f-exists?
              (expand-file-name
               ".clang-format"
               (projectile-project-root)))
             (not
              (f-exists?
              (expand-file-name
               ".noformat"
               (projectile-project-root)))))
    (lsp-format-buffer)))

(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook (((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                    (require 'ccls)
                                                    (lsp)))
         ((c-mode c++-mode) . (lambda ()
                                (add-hook 'before-save-hook 'clang-format-buffer-smart nil t))))
  :config
  (dolist (dir '(".ccls-cache" "build" "googletest"))
    (add-to-list 'lsp-file-watch-ignored dir))

  (define-advice c-clear-string-fences (:around (fn) inhibit-args-out-of-range-error)
    (ignore-errors
      (funcall fn)))
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t) :xref (:container t)
                      :diagnostics (:frequencyMs 5000)))
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (setq lsp-enabled-clients '(pyright))
                         (lsp)))
  :config
  (setq lsp-pyright-auto-import-completions nil))

(use-package lsp-haskell
  :hook ((haskell-mode . (lambda ()
                           (require 'lsp-haskell)
                           (lsp))))
  :init
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(provide 'init-lsp)
