;; -- General variables --

(setq-default
 avy-all-windows        'all-frames
 ;; doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-font              (font-spec :family "Inconsolata" :size 14 :weight 'regular)
 doom-theme             'doom-vibrant
 which-key-idle-delay   0.2

 ;;;; WEB JS AND WHATEVER STUFF
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


;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff --

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.
