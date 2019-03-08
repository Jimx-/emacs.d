(defvar after-make-console-frame-hook '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hook '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hook' or
`after-make-window-system-frame-hook'"
  (message "run hooks")
  (with-selected-frame frame
    (run-hooks (if (display-graphic-p)
                   'after-make-window-system-frame-hook
                 'after-make-console-frame-hook))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst sanityinc/initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when sanityinc/initial-frame
                  (run-after-make-frame-hooks sanityinc/initial-frame))))


(provide 'init-frame-hooks)
