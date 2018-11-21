(use-package org
  :ensure nil
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCEL(c)"))
        org-startup-indented t)

  ;; Fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

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
                               (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages load-language-list)

  ;; Preview HTML
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Capture templates
  (setq org-capture-templates
        `(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Web Links")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Web Links")
           "* %? [[%:link][%:description]] :link:\n")))

  ;; Publishing
  (setq org-publish-project-alist
        '(("org-jimx"
           ;; Path to your org files.
           :base-directory "~/org/blog"
           :base-extension "org"

           ;; Path to your Jekyll project.
           :publishing-directory "~/projects/jekyll/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t ;; Only export section between <body> </body>
           )


          ("org-static-jimx"
           :base-directory "~/org/blog/assets"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/projects/jekyll/assets"
           :recursive t
           :publishing-function org-publish-attachment)

          ("jimx" :components ("org-jimx" "org-static-jimx")))))

(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs path))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"%s\" alt=\"%s\"/>" (substring path 2) desc))))

(add-hook 'org-load-hook (lambda () (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)) t)

(provide 'init-org)
