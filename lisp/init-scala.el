;; -*- lexical-binding: t -*-

(use-package scala-mode
  :config
  (setq scala-indent:align-parameters t
        scala-indent:use-javadoc-style t)
  (set-pretty-symbols 'scala-mode
    ;; Functional
    :def "def"
    :lambda "Lambda"
    ;; Types
    :null "none"
    :null "None"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Boolean"
    :list "List"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :yield "yield"))

(provide 'init-scala)
