(use-package racket-mode
  :config
  (set-pretty-symbols 'racket-mode
    :lambda "lambda"
    :map "map"
    :dot "dot")
  (setq racket-smart-open-bracket-enable t))

(provide 'init-racket)
