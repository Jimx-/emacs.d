;;; -*- lexical-binding: t -*-
(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch)

         :map counsel-mode-map
         ("C-x C-r" . counsel-recentf)

         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-immediate-done)
         ("C-RET" . ivy-immediate-done)
         ("<up>" . ivy-previous-line-or-history))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq ivy-use-virtual-buffers nil
        ivy-format-function #'ivy-format-function-line
        ivy-initial-inputs-alist nil
        ivy-use-selectable-prompt t
        ivy-count-format "(%d/%d) ")

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Ignore hidden files
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Enhance fuzzy matching
  (use-package flx)

  ;; Enhance M-x
  (use-package amx)

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :defines (all-the-icons-dir-icon-alist bookmark-alist)
    :functions (all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :preface
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (when-let* ((buffer (get-buffer candidate))
                    (major-mode (buffer-local-value 'major-mode buffer))
                    (icon (if (and (buffer-file-name buffer)
                                   (all-the-icons-auto-mode-match? candidate))
                              (all-the-icons-icon-for-file candidate)
                            (all-the-icons-icon-for-mode major-mode))))
          (if (symbolp icon)
              (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let ((icon (if (file-directory-p candidate)
                        (cond
                         ((and (fboundp 'tramp-tramp-file-p)
                               (tramp-tramp-file-p default-directory))
                          (all-the-icons-octicon "file-directory"))
                         ((file-symlink-p candidate)
                          (all-the-icons-octicon "file-symlink-directory"))
                         ((all-the-icons-dir-is-submodule candidate)
                          (all-the-icons-octicon "file-submodule"))
                         ((file-exists-p (format "%s/.git" candidate))
                          (all-the-icons-octicon "repo"))
                         (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                              (apply (car matcher) (list (cadr matcher))))))
                      (all-the-icons-icon-for-file candidate))))
          (unless (symbolp icon)
            (propertize icon
                        'face `(
                                :height 1.1
                                :family ,(all-the-icons-icon-family icon)
                                ))))))
    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name))))
    :init
    (setq ivy-rich-display-transformers-list
          '(ivy-switch-buffer
            (:columns
             ((ivy-rich-buffer-icon)
              (ivy-rich-candidate (:width 30))
              (ivy-rich-switch-buffer-size (:width 7))
              (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
              (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
              (ivy-rich-switch-buffer-project (:width 15 :face success))
              (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
             :predicate
             (lambda (cand) (get-buffer cand)))
            counsel-M-x
            (:columns
             ((counsel-M-x-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-function
            (:columns
             ((counsel-describe-function-transformer (:width 40))
              (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
            counsel-describe-variable
            (:columns
             ((counsel-describe-variable-transformer (:width 40))
              (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
            counsel-recentf
            (:columns
             ((ivy-rich-file-icon)
              (ivy-rich-candidate (:width 0.7))
              (ivy-rich-file-last-modified-time (:face font-lock-comment-face))
              ))))

    (setq ivy-rich-parse-remote-buffer nil)
    (ivy-rich-mode 1)
    (ivy-posframe-mode 1))


  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :init (counsel-projectile-mode 1)))

;; Use posframe to show candidates
(use-package ivy-posframe
  :if (> emacs-major-version 25)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-parameters
        `((min-width . 70)
          (min-height . ,ivy-height)
          (internal-border-width . 2))
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))

  ;; posframe does not work well with mouse
  ;; (setq posframe-mouse-banish t)

  (dolist (fn '(swiper counsel-ag counsel-grep counsel-git-grep))
    (setf (alist-get fn ivy-display-functions-alist) #'ivy-display-function-fallback)))

(provide 'init-ivy)
