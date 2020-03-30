;;; ~/Development/dotfiles/doom/+funcs.el -*- lexical-binding: t; -*-


(defun tees/align-& (start end)
    "Align columns by ampersand"
    (interactive "r")
    (align-regexp start end "\\(\\s-*\\)&" 1 1 t))

(defun tees/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t))

(defun tees/write ()
  (interactive)
  ;; (setq buffer-face-mode-face '(:family "iA Writer Duospace" :height 140)) ; set the font
  (setq
    writeroom-width         90    ; set width of writeroom mode
    indent-tabs-mode        t     ; use tabs for indentation
    tab-width               2     ; set tab width to 2 FIXME
    writeroom-mode-line     nil   ; don't show the modeline
    truncate-lines          nil   ; wrap lines?
    line-spacing            5     ; set line spacing
    global-hl-line-mode     nil   ; Turn off line highlight
    display-line-numbers    nil)  ; don't show line numbers
  (fringe-mode              0)    ; don't show fringe.
  (set-fill-column          90)   ; set width of fill column (for text wrapping.)
  (auto-fill-mode           0)    ; disable line breaking.
  (company-mode             0)    ; disable completion.
  (linum-mode               0)    ; turn off  line  numbers (dooum style.)
  (global-linum-mode        0)    ; turn off  line  numbers again.
  (hl-line-mode             0)    ; I said stop  highlighting stuff!
  (writeroom-mode           1)    ; go into write room   mode.
  (visual-line-mode         1)    ; don't know.
  (blink-cursor-mode)                      ; let's blink that cursor.
  (run-at-time "1 sec" nil #'toggle-frame-fullscreen))  ; no fullscreen!
  

(defun tees/todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))


(defun tees/org-hook ()
  (interactive)
  (set-face-attribute 'org-level-1 nil :height 1.0 :background nil))

(add-hook 'org-load-hook #'tees/org-hook)
