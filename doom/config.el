;; -- General variables --

(setq-default
 avy-all-windows        'all-frames
 doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-theme             'doom-solarized-light
 which-key-idle-delay   0.2
 )


(toggle-frame-maximized)

;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff --

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.
