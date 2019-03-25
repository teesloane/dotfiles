(setq-default
 avy-all-windows               'all-frames
 doom-font                     (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-theme                    'doom-nord
 which-key-idle-delay          0.2
 ;; Web mode things.
 ;; js2-bounce-indent-p           nil
 ;; js2-highlight-level           3
 ;; js2-basic-offset              2
 ;; js-indent-level               2
 ;; web-mode-markup-indent-offset 2
 ;; web-mode-css-indent-offset    2
 ;; web-mode-code-indent-offset   2
 ;; web-mode-style-padding        2
 ;; web-mode-script-padding       2
 ;; css-indent-offset             2
 )


;; (load! "+org")
;; (load! "+bindings")


(defun bjm/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun bjm/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))
