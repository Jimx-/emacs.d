;; -*- lexical-binding: t -*-

(require 'init-custom)

(use-package citar
  :if (eq custom-completion-frontend 'vertico)
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :config
  (setq citar-notes-paths (list (expand-file-name "papers" custom-org-directory)))

  (setq citar-templates
        `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, \
${journal journaltitle publisher container-title collection-title}.\n")
          (note . ,(concat "${title}\n"
                           "* TODO Notes\n"
                           ":PROPERTIES:\n"
                           ":Custom_ID: ${=key=}\n"
                           ":NOTER_DOCUMENT: ${file}\n"
                           ":AUTHOR: ${author}\n"
                           ":JOURNAL: ${journaltitle}\n"
                           ":DATE: ${date}\n"
                           ":YEAR: ${year}\n"
                           ":DOI: ${doi}\n"
                           ":URL: ${url}\n"
                           ":END:"))))

  (when custom-latex-bibtex-file
    (setq citar-bibliography (list (expand-file-name custom-latex-bibtex-file)))))

(use-package bibtex-completion
  :if (eq custom-completion-frontend 'ivy)
  :config
  (setq bibtex-completion-bibliography (list custom-latex-bibtex-file)
        bibtex-completion-notes-path (expand-file-name "papers/notes.org" custom-org-directory)))

(use-package ivy-bibtex
  :if (eq custom-completion-frontend 'ivy)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key
        bibtex-completion-notes-path (expand-file-name "papers" custom-org-directory)
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+TITLE: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: ${file}\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n"
         ))

  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "zathura" nil 0 nil fpath)))

  (when custom-latex-bibtex-file
    (setq bibtex-completion-bibliography (list (expand-file-name custom-latex-bibtex-file)))))

(provide 'init-biblio)
