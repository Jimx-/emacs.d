;; -*- lexical-binding: t -*-
(setq debug-on-error nil)


(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-benchmarking)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-custom)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-package)   ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

(require 'init-basic)
(require 'init-ui)
(require 'init-utils)
(require 'init-highlight)
;; (require 'init-evil)

(require 'init-edit)
(require 'init-windows)
(require 'init-dired)
(require 'init-prettify)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-grep)
(require 'init-shell)

(cond ((eq custom-completion-frontend 'vertico)
       (require 'init-vertico))
      (t (require 'init-ivy)))

(require 'init-company)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-lsp)

(require 'init-mail)
(require 'init-rss)

(require 'init-cc)
(require 'init-lisp)
(require 'init-python)
(require 'init-racket)
(require 'init-org)
(require 'init-markdown)
(require 'init-web)
(require 'init-elixir)
(require 'init-haskell)
(require 'init-idris)
(require 'init-go)
(require 'init-latex)
(require 'init-rust)
(require 'init-scala)

(require 'init-biblio)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(if (file-exists-p custom-file)
    (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
(put 'upcase-region 'disabled nil)
