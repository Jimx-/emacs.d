(use-package cc-mode
  :ensure nil
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (setq-default c-backspace-function #'delete-backward-char
                c-default-style "stroustrup")
  :config
  ;; Style/formatting
  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (c-set-offset 'substatement-open '0)  ; don't indent brackets
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  (c-set-offset 'func-decl-cont '0)
  (c-set-offset 'member-init-cont '-)
  (c-set-offset 'brace-list-open '+)
  (c-set-offset 'case-label '0)
  (c-set-offset 'access-label '-)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'innamespace '0))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))
