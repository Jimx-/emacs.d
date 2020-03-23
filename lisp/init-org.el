(use-package org
  :ensure nil
  ;; Use variable pitch font when writing prose in Org-mode
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))

  :config
  (setq org-directory "~/org/"
        org-agenda-files (ignore-errors (directory-files-recursively org-directory "^\\(_.*\\|ref\\)\\.org$" t))
        org-attach-id-dir (concat org-directory ".attach/")
        org-attach-store-link-p t
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)"))
        org-startup-indented t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))

  ;; Syntax highlight for code segment
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-latex-minted-options
        '(("linenos=true") ("breaklines")))

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
                                               org-meta-line org-document-info-keyword))
                              (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))

  ;; Pretty symbols
  (set-pretty-symbols 'org-mode
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"
    :checkbox-checked "[X]"
    :checkbox-empty "[ ]"
    :checkbox-inter "[-]")

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

  ;; Handle non-image files a little differently. Images should be inserted
  ;; as-is, as image previews. Other files, like pdfs or zips, should be linked
  ;; to, with an icon indicating the type of file.
  (advice-add 'org-download-insert-link :override #'org-custom-download-insert-link))

(defun org-custom-download-insert-link (_link filename)
  (if (looking-back "^[ \t]+" (line-beginning-position))
        (delete-region (match-beginning 0) (match-end 0))
      (newline))
    (cond ((image-type-from-file-name filename)
           (insert
            (concat (if (= org-download-image-html-width 0) ""
                      (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
                    (if (= org-download-image-latex-width 0) ""
                      (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
                    (cond ((file-in-directory-p filename org-attach-directory)
                           (format "[[file:%s]]" filename))
                          ((file-in-directory-p filename org-directory)
                           (format org-download-link-format (file-relative-name filename org-directory)))
                          ((format org-download-link-format filename)))))
           (org-display-inline-images))
          ((insert
            (format "[[./%s][%s]] "
                    (file-relative-name filename (file-name-directory buffer-file-name))
                    (file-name-nondirectory (directory-file-name filename)))))))

(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"%s\" alt=\"%s\"/>" (substring path 2) desc))))

(add-hook 'org-load-hook (lambda () (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)) t)

(use-package org-download
  :after org
    :bind (:map org-mode-map
              ("<f2>" . org-download-screenshot))
  :config
  (setq org-download-image-dir org-attach-id-dir
        org-download-method 'attach
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-screenshot-file (concat temporary-file-directory "screenshot.png")
        org-download-screenshot-method
        (cond ((executable-find "maim")  "maim -s %s")
              ((executable-find "scrot") "scrot -so %s")))
  (org-download-enable))

(provide 'init-org)
