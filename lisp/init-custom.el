;; -*- lexical-binding: t -*-

;; If you don't customize it, this is the theme you get.
(defvar custom-enabled-theme 'leuven
  "A symbol representing the enabled theme.")

(defvar custom-font nil
  "The default font.")

(defvar custom-unicode-font nil
  "Fallback font for unicode glyph.")

(defvar custom-variable-pitch-font nil
  "The default font for variable-pitch text.")

(defcustom custom-org-directory (expand-file-name "~/org/")
  "Set org directory."
  :type 'string)

(defvar custom-latex-bibtex-file nil
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defcustom custom-completion-frontend 'ivy
  "Completion scheme."
  :type '(choice (const :tag "Ivy" ivy)
                 (const :tag "Vertico" vertico)))

(provide 'init-custom)
