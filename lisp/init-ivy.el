;;; -*- lexical-binding: t -*-
(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper)

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
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) ")

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (use-package flx)
  (use-package amx)

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :init (ivy-rich-mode 1)
    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
				   (or (and ivy-rich-mode 'abbreviate) 'name)))))
  
  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
  
  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :init (counsel-projectile-mode 1)))

(when (> emacs-major-version 25)
  (use-package ivy-posframe
    :hook (ivy-mode . ivy-posframe-enable)
    :config
    (setq ivy-fixed-height-minibuffer nil
          ivy-posframe-parameters
          `((min-width . 70)
            (min-height . ,ivy-height)
	    (internal-border-width . 10))
          ivy-display-function #'ivy-posframe-display-at-window-center)))

(provide 'init-ivy)
