;; -- General variables --

(setq-default
 avy-all-windows        'all-frames
 ;; doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-font              (font-spec :family "Inconsolata" :size 14 :weight 'regular)
 doom-theme             'doom-one
 which-key-idle-delay   0.2
 global-whitespace-mode 0
 whitespace-mode 0

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

;; -- Custom Bindings ----------------------------------------------------------
;;
;;

(map!
 ;; --- <GLOBAL> -------------------------------------

 ;; --- <LEADER> -------------------------------------
 (:leader
   (:desc "tees" :prefix "v"
     :desc "M-X Alt"             :n "v" #'execute-extended-command
		 )

   (:desc "lisp" :prefix "k"
     :desc "sp-copy"              :n "c" #'sp-copy-sexp
		 :desc "sp-kill"              :n "k" #'sp-kill-sexp
     :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
     :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
     :desc "sp-up"                :n "u" #'sp-up-sexp
     :desc "sp-down"              :n "d" #'sp-down-sexp
     :desc "sp-next"              :n "l" #'sp-next-sexp
     :desc "sp-prev"              :n "h" #'sp-previous-sexp)

   ))


;; -- Hooks ------------------------

(add-hook! 'doom-load-theme-hook
  (after! outline
    (set-face-attribute 'outline-1 nil :height 1.0 :background nil)))


;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff --

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.
