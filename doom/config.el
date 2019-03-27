(setq-default
  avy-all-windows        'all-frames
  doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
  doom-theme             'doom-nord
  which-key-idle-delay   0.2
 )

;; (load! "+org")
;; (load! "+bindings")


(defun tees/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

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
