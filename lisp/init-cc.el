;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :commands (c-mode c++-mode objc-mode java-mode)
  :init
  (setq-default tab-width 4
                c-basic-offset tab-width
                c-default-style "stroustrup")
  :config
  ;; Pretty symbols
  (set-pretty-symbols '(c-mode c++-mode)
    :null "nullptr"
    :true "true" :false "false"
    :int "int" :str "std::string"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "!"
    :and "and" :or "or"
    :for "for"
    :return "return")

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
  (c-set-offset 'innamespace '0)

  (setq c-tab-always-indent nil
        c-electric-flag nil)

  (dolist (key '("#" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
    (define-key c-mode-base-map key nil))

  (dolist (key '("<" ">"))
    (define-key c++-mode-map key nil)))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;; Clang-format emacs integration for use with C/Objective-C/C++.
(use-package clang-format
  :bind (("C-c i" . clang-format-region)
         ("C-c u" . clang-format-buffer))
  :config
  (setq clang-format-style-option "llvm"))

(provide 'init-cc)
