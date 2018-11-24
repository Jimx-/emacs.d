(use-package lsp-mode
  :diminish lsp-mode
  :config
  (setq lsp-message-project-root-warning t
        create-lockfiles nil)

  ;; Restart server/workspace in case the lsp server exits unexpectedly.
  ;; https://emacs-china.org/t/topic/6392
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro org-babel-lsp (lang &optional enable-name)
    "Support LANG in org source code block. "
    (cl-check-type lang string)
    (cl-check-type enable-name (or null string))
    (let ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
          (client (intern (format "lsp-%s-enable" (or enable-name lang)))))
      `(progn
         (defun ,edit-pre (babel-info)
           (let ((lsp-file (or (->> babel-info caddr (alist-get :file))
                               buffer-file-name)))
             (setq-local buffer-file-name lsp-file)
             (setq-local lsp-buffer-uri (lsp--path-to-uri lsp-file))
             (,client)))
         (put ',edit-pre 'function-documentation
              (format "Prepare local buffer environment for org source block (%s)."
                      (upcase ,lang))))))

  ;; FIXME: https://github.com/emacs-lsp/lsp-python/issues/28
  (defun lsp--suggest-project-root ()
    "Get project root."
    (or
     (when (featurep 'projectile) (projectile-project-root))
     (when (featurep 'project)
       (when-let ((project (project-current)))
         (car (project-roots project))))
     default-directory))

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  :config
  (setq lsp-ui-doc-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'top
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil))

(use-package company-lsp
  :after company
  :defines company-backends
  :functions company-backend-with-yas
  :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable)
  :config (org-babel-lsp "python"))

;; C/C++/Objective-C lang server support for lsp-mode using clang
;; Install: yaourt ccls
;;          refer to  https://github.com/MaskRay/ccls/wiki/Getting-started
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :commands lsp-ccls-enable
  :hook ((c-mode c++-mode objc-mode) . lsp-ccls-enable)
  :config
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t) :xref (:container t)
                      :diagnostics (:frequencyMs 5000)))
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))

;; Racket support for lsp-mode
(use-package lsp-racket
  :disabled
  :commands lsp-racket-enable
  :hook (racket-mode . lsp-racket-enable))

(provide 'init-lsp)
