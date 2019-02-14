;; Menubar mods.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; all comments are slanted.
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic)

;; Defaults
(setq-default
 avy-all-windows                'all-frames
 company-idle-delay             0.2
 company-minimum-prefix-length  3
 doom-font                      (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-theme                     'doom-nord
 which-key-idle-delay           0.2
 js2-bounce-indent-p nil
 js2-highlight-level 3
 js2-basic-offset 2
 js-indent-level 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-style-padding 2
 web-mode-script-padding 2
 css-indent-offset 2
 )


(load! "+org")
(load! "+bindings")
