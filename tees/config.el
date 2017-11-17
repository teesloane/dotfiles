;;;; private/tees/config.el -*- lexical-binding: t; -*-

(load! +bindings)

;; some fonts
;; (set-face-attribute 'default nil :font "Fantasque Sans Mono-13")
(set-face-attribute 'default nil :font "Inconsolata-14")
;; (set-face-attribute 'default nil :font "Fira Code-12")
;; (set-face-attribute 'default nil :font "Hack-12")



;; MODES
(def-package! flx :demand t)
(def-package! prettier-js    :mode "\\.js$"           :config)
(def-package! js-import      :commands js-import      :config)
(def-package! rjsx-mode      :commands rjsx-mode      :config)
(def-package! writeroom-mode :commands writeroom-mode :config)

;; Setup modes as necessary (ugh something should change here...)
(push '("\\.js\\'"   . rjsx-mode)            auto-mode-alist)
(push '("\\.css\\'"  . web-mode)             auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)            auto-mode-alist)
(push '("\\.clj\\'"  . clojure-mode)         auto-mode-alist)
(push '("\\.cljs\\'" . clojurescript-mode)   auto-mode-alist)
(push '("\\.http\\'" . restclient-mode)      auto-mode-alist)

;; Org setup

;;;;;;;;;;;;;;;
;; ORG STUFF ;;
;;;;;;;;;;;;;;;

;; doom stuff
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

;; make org headings all the same size.
(defun my/org-mode-hook ()
  (set-face-attribute 'org-level-1 nil :height 1.0 :background nil)
  (set-face-attribute 'org-ellipsis nil :height 1.0 :background nil))
(add-hook 'org-load-hook #'my/org-mode-hook)


(add-to-list 'load-path "./stuff/orgmode-mediawiki")
(load! +orgmode-mediawiki)

;; NEOTREE ;;
;; icon size
(defun text-scale-twice ()(interactive)(progn(text-scale-adjust 0)(text-scale-decrease 1.0)))
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))
;; window config
(setq neo-window-width 30)
(setq neo-window-fixed-size nil)


;; DEFAULTS
(setq-default
 ;; GENERAL STUFF
 line-spacing 0.4
 tab-width 2
 indent-tab-mode nil
 which-key-idle-delay 0.3

 ;;;; JAVASCRIPT STUFF
 js2-bounce-indent-p nil
 js2-highlight-level 3
 js2-basic-offset 2
 js-indent-level 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-style-padding 2
 web-mode-script-padding 2

 ;; PLUGINS
 avy-all-windows t
)

;; Local Vars? ;;

 ;; make ivy fuzzy search
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

;; stop rg from crashing with loooooooooooooooooong files.
(setq counsel-rg-base-command
      "rg -i -M 160 --no-heading --line-number --color never %s .")

;; Auto Completion
(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;;;; hooks
(defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
