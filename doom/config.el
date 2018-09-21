;; lang/org

;; UI
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic)

;; PACKAGES AND MODES ----------------------------------------------------------

(def-package! prettier-js :mode "\\.js$" :config)
(def-package! js-import :commands js-import :config)
(def-package! writeroom-mode :commands writeroom-mode :config)

(def-package! deft
  :demand t
  :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :demand t
  :config
  (setq avy-all-windows t avy-background t))

;; note to self: C-x C-f for company file auto complete
(require 'company)

(push '("\\.js\\'"   . rjsx-mode)   auto-mode-alist)
(push '("\\.css\\'"  . web-mode)    auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)   auto-mode-alist)
(push '("\\.rkt\\'" .  scheme-mode) auto-mode-alist)


;; DEFAULTS --------------------------------------------------------------------

;; menu bar thing.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default
 ;; GENERAL STUFF
 line-spacing 0.1
 tab-width 2
 indent-tab-mode nil
 which-key-idle-delay 0.2
 evil-ex-search-case 'sensitive
 auto-window-vscroll nil ;; apparently this slows down emacs if true
 +pretty-code-iosevka-ligatures-enabled-by-default t

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

 ;; PLUGINS
 global-prettify-symbols-mode t
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 company-idle-delay 0.2
 company-minimum-prefix-length 3
 neo-window-width 30
 neo-window-fixed-size nil
 deft-auto-save-interval 20
 avy-all-windows 'all-frames
 )


 ;; MODES + HOOKS + FUNCTIONS --------------------------------------------------

(load! "+funcs")

(add-hook 'web-mode-hook     'tees/web-mode-hook)
(add-hook 'org-load-hook     'tees/org-mode-hook)
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'neotree-text-size)))


;; ---- File / Modules

(load! "+org")
(load! "+bindings")
