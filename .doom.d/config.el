;; -- General variables --

(setq-default
 avy-all-windows        'all-frames
 ;; doom-font              (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-font              (font-spec :family "Inconsolata" :size 15 :weight 'regular)
 doom-theme             'doom-gruvbox
 which-key-idle-delay   0.2
 global-whitespace-mode 0
 whitespace-mode 0
 global-whitespace-mode 0

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

 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.

 )

;;
;; -- Custom Bindings ----------------------------------------------------------doom-one
;;

(map!
 ;; -- <GLOBAL> ---------------------------------------------------------------

 :desc "Switch to 1st workspace"   "s-1"   (λ! (+workspace/switch-to 0))
 :desc "Switch to 2nd workspace"   "s-2"   (λ! (+workspace/switch-to 1))
 :desc "Switch to 3rd workspace"   "s-3"   (λ! (+workspace/switch-to 2))
 :desc "Switch to 4th workspace"   "s-4"   (λ! (+workspace/switch-to 3))
 :desc "Switch to 5th workspace"   "s-5"   (λ! (+workspace/switch-to 4))
 :desc "Switch to 6th workspace"   "s-6"   (λ! (+workspace/switch-to 5))
 :desc "Switch to 7th workspace"   "s-7"   (λ! (+workspace/switch-to 6))
 :desc "Switch to 8th workspace"   "s-8"   (λ! (+workspace/switch-to 7))
 :desc "Switch to 9th workspace"   "s-9"   (λ! (+workspace/switch-to 8))
 :desc "Create workspace"          "s-t"   (λ! (+workspace/new))

 ;; -- <LEADER> ----------------------------------------------------------------
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
   )
 )

;; -- Hooks --------------------------------------------------------------------

(add-hook! 'doom-load-theme-hook
  (after! outline
    (set-face-attribute 'outline-1 nil :height 1.0 :background nil)))

;; -- Local file requires --

(load! "+funcs")
(load! "+org")

;; -- Enable gpg stuff ---------------------------------------------------------

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.
