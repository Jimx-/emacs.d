;; -*- lexical-binding: t -*-

(require 'init-custom)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))

(use-package tex
  :ensure auctex
  :hook ((TeX-mode . variable-pitch-mode)
         (TeX-mode . visual-line-mode)
         (TeX-mode . TeX-fold-mode)
         (TeX-mode . rainbow-delimiters-mode)
         (TeX-mode . LaTeX-math-mode)
         (TeX-mode . flyspell-mode)
         (TeX-mode . turn-on-auto-fill))
  :init
  (setq TeX-parse-self t
        TeX-auto-save t)

  (add-hook 'TeX-mode-hook (lambda () (setq cursor-type 'bar)))

  (add-hook 'TeX-mode-hook
            (lambda ()
              (company-auctex-init)
              (setcar company-backends
                      (let ((backend (car company-backends)))
                        (funcall (if (consp backend) 'append 'cons)
                                 backend '(company-math-symbols-unicode
                                           company-math-symbols-latex
                                           company-reftex-citations
                                           company-reftex-labels))))))

  :config
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil)

  (when (executable-find "zathura")
    (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))
  ;; (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  ;; (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane"))


  ;; Fontification taken from https://tex.stackexchange.com/a/86119/81279
  (setq font-latex-match-reference-keywords
        '(;; biblatex
          ("printbibliography" "[{")
          ("addbibresource" "[{")
          ;; Standard commands
          ("cite" "[{")
          ("citep" "[{")
          ("citet" "[{")
          ("Cite" "[{")
          ("parencite" "[{")
          ("Parencite" "[{")
          ("footcite" "[{")
          ("footcitetext" "[{")
          ;; Style-specific commands
          ("textcite" "[{")
          ("Textcite" "[{")
          ("smartcite" "[{")
          ("Smartcite" "[{")
          ("cite*" "[{")
          ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists
          ("cites" "[{")
          ("Cites" "[{")
          ("parencites" "[{")
          ("Parencites" "[{")
          ("footcites" "[{")
          ("footcitetexts" "[{")
          ("smartcites" "[{")
          ("Smartcites" "[{")
          ("textcites" "[{")
          ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands
          ("autocite" "[{")
          ("Autocite" "[{")
          ("autocite*" "[{")
          ("Autocite*" "[{")
          ("autocites" "[{")
          ("Autocites" "[{")
          ;; Text commands
          ("citeauthor" "[{")
          ("Citeauthor" "[{")
          ("citetitle" "[{")
          ("citetitle*" "[{")
          ("citeyear" "[{")
          ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands
          ("fullcite" "[{")
          ;; cleveref
          ("cref" "{")
          ("Cref" "{")
          ("cpageref" "{")
          ("Cpageref" "{")
          ("cpagerefrange" "{")
          ("Cpagerefrange" "{")
          ("crefrange" "{")
          ("Crefrange" "{")
          ("labelcref" "{")))

  (setq font-latex-match-textual-keywords
        '(;; biblatex brackets
          ("parentext" "{")
          ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary Commands
          ("textelp" "{")
          ("textelp*" "{")
          ("textins" "{")
          ("textins*" "{")
          ;; subcaption
          ("subcaption" "[{")))

  (setq font-latex-match-variable-keywords
        '(;; amsmath
          ("numberwithin" "{")
          ;; enumitem
          ("setlist" "[{")
          ("setlist*" "[{")
          ("newlist" "{")
          ("renewlist" "{")
          ("setlistdepth" "{")
          ("restartlist" "{")
          ("crefname" "{")))
  )

(use-package cdlatex
  :hook ((LaTeX-mode . cdlatex-mode)
         (org-mode . org-cdlatex-mode)))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-toc-split-windows-horizontally t
        reftex-plug-into-AUCTeX t)

  (when custom-latex-bibtex-file
    (setq reftex-default-bibliography (list (expand-file-name custom-latex-bibtex-file)))))

(use-package company-auctex)
(use-package company-reftex)
(use-package company-math)

(use-package latex-preview-pane
  :config
  (setq latex-preview-pane-multifile-mode 'auctex))

(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))

(provide 'init-latex)
