;; -*- lexical-binding: t -*-

(use-package python
  :ensure nil
  :defer t
  :config
  (setq python-shell-completion-native-enable nil)
  (set-pretty-symbols 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield"))

;; Environment management
(use-package pipenv
  :commands (pipenv-project-p)
  :hook (python-mode . pipenv-mode))

;; Yapf for Emacs
;; Install: pip install yapf
(use-package yapfify
  :quelpa (yapfify :fetcher github :repo "JorisE/yapfify")
  :hook (python-mode . yapf-mode))

;; py-isort.el integrates isort into Emacs.
;; Install: pip install isort
(use-package py-isort)

(provide 'init-python)
