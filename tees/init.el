;;; private/tees/init.el -*- lexical-binding: t; -*-

(set-default-font "Fira Mono 12")
(setq which-key-idle-delay 0.3 )


;; SOME FUNCTIONS
(defun tees/line-height (x)
  "Sets line height"
  (interactive)
  (setq line-spacing 0.8)
  )

(setq-default
  initial-major-mode 'org-mode
  initial-scratch-message
  "// This buffer is for ES notes you don't want to save.\n\n")
