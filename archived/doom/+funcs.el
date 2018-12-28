
(defun tees/init-diff ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files
   "~/.config/doom/init.el"
   "~/.emacs.d/init.example.el"))

;; Align funcs ripped from http://pragmaticemacs.com/emacs/aligning-text/

(defun tees/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun tees/max-buffer ()
  "Current buffer becomes full width"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun tees/clip-file ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (x-select-text filename))))

(defun neotree-text-size ()
  "Change neotree textsize."
  (interactive)
  (progn (text-scale-adjust 0)(text-scale-decrease 0)))

(defun tees/org-mode-hook ()
  "Setup my org mode to do it's magic. Aligns tags, change heading sizes / backgrounds."
  (interactive)
  (setq org-tags-column 80)
  (org-align-all-tags)
  (dolist (level '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6))
    (set-face-attribute level nil :height 1.0 :background nil))
  (set-face-attribute 'org-level-1 nil :height 0.8 :background nil)
  )

(defun tees/web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset    2
   web-mode-code-indent-offset   2
   web-mode-style-padding        2
   web-mode-script-padding       2))

(defun tees/clojure-indent ()
  (interactive)
  (setq clojure-defun-style-default-indent
        (if (eq t clojure-defun-style-default-indent) nil 't))
  (message "Clojure default indent set to: %s"
           (if (eq t clojure-defun-style-default-indent) "true" "false")))
