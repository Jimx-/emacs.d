(use-package org
  :ensure nil
  ;; Use variable pitch font when writing prose in Org-mode
  :hook (org-mode . variable-pitch-mode)
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)"))
        org-startup-indented t
        org-hide-emphasis-markers t
        org-hide-leading-stars t)

  ;; Fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; Use bar cursor under
  (add-hook 'org-mode-hook '(lambda () (setq cursor-type 'bar)))

  ;; Use fixed pitch font for org-block, etc.
  (add-hook 'org-mode-hook '(lambda ()
                              (mapc (lambda (face)
                                      (set-face-attribute face nil :inherit 'fixed-pitch))
                                    '(org-code org-block org-table org-link org-verbatim
                                               org-block-begin-line org-block-end-line
                                               org-meta-line org-document-info-keyword
                                               org-indent))))

  ;; Pretty symbols
  (set-pretty-symbols 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC")

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (dot . t)
                               (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)

  ;; Preview HTML
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Convert buffer text and decorations to HTML
  (use-package htmlize))

(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"%s\" alt=\"%s\"/>" (substring path 2) desc))))

(add-hook 'org-load-hook (lambda () (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)) t)

(provide 'init-org)
