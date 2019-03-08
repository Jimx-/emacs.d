(use-package vterm
  :ensure nil
  :load-path (lambda () (list (concat user-emacs-directory "/quelpa/build/vterm")))
  :quelpa (vterm :fetcher github :repo "akermu/emacs-libvterm")
  :init
  (unless (file-executable-p (concat (file-name-directory (locate-library "vterm"))
                                     "vterm-module.so"))
    (setq-default vterm-install t))
  :when (and (string-match-p "MODULES" system-configuration-features)
             (display-graphic-p)))

(provide 'init-shell)
