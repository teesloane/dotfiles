(setq-default
 avy-all-windows        'all-frames
 doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 ;; doom-font              (font-spec :family "iA Writer Duospace" :size 13 :weight 'regular)
 doom-theme             'doom-nord
 which-key-idle-delay   0.2
 )

;; (load! "+org")
;; (load! "+bindings")

;; Enable gpg stuff...
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)

;; -- Funcs --------------------------------

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

;; (add-hook 'markdown-mode-hook 'tees/write)

(defun tees/write ()
  (interactive)
  (setq buffer-face-mode-face '(:family "iA Writer Duospace" :height 124)) ; set the font
  (setq writeroom-width 80)                                                ; set width of writeroom mode
  (set-fill-column 80)                                                     ; set width of fill column (for text wrapping.)
  (setq-default indent-tabs-mode t)                                        ; use tabs for indentation
  (setq-default tab-width 1)                                               ; set tab width to 2 FIXME
  (setq writeroom-mode-line nil)                                          ; don't show the modeline
  (setq truncate-lines nil)                                                ; wrap lines?
  (setq line-spacing 5)                                                    ; set line spacing
  (setq global-hl-line-mode nil)                                           ; Turn off line highlight
  (setq display-line-numbers nil)                                          ; don't show line numbers
  (setq ns-right-alternate-modifier 'none) ; for the joy of creating em-dashes with the right option key. Dammit.
  (fringe-mode 0)                                                          ; don't show fringe.
  (buffer-face-mode)                                                       ; ?
  (linum-mode 0)                                                           ; turn off line numbers (dooum style.)
  (global-linum-mode 0)                                                    ; turn off line numbers again.
  (writeroom-mode 1)                                                       ; go into write room mode.
  (blink-cursor-mode)                                                      ; let's blink that cursor.
  (visual-line-mode 1)                                                     ; don't know.
  (run-at-time "1 sec" nil #'turn-off-solaire-mode)                        ; lol basically set timeout b/c a hook somewhere is turning on solaire mode
  (hl-line-mode 0)                                                         ; I said stop highlighting stuff!
  (load-theme 'doom-one)                                       ; make it dark.
  )

(defun tees/todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))
