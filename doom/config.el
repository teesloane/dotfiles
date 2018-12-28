
;; Menubar mods.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


;; Defaults
(setq-default
 which-key-idle-delay 0.2
 company-idle-delay 0.2
 company-minimum-prefix-length 3
 avy-all-windows 'all-frames

 )


(load! "+bindings")
