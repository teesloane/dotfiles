;;;; private/tees/config.el -*- lexical-binding: t; -*-

(load! +bindings)

;; some fonts
(set-face-attribute 'default nil :font "IBM Plex Mono-12")
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic )


;; MODES
(def-package! flx :demand t) ;; this is for fuzzy searching in ivy i think.
(def-package! prettier-js    :mode "\\.js$"           :config)
(def-package! js-import      :commands js-import      :config)
(def-package! writeroom-mode :commands writeroom-mode :config)


;; Setup modes as necessary (ugh something should change here...)
(push '("\\.js\\'"   . rjsx-mode)            auto-mode-alist) ;; bummer. should fix this someday
(push '("\\.css\\'"  . web-mode)             auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)            auto-mode-alist)

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
  (set-face-attribute 'org-ellipsis nil :height 1.0 :background nil)
  (add-to-list 'load-path "./stuff/orgmode-mediawiki")
  (setq org-tags-column 80)
  (org-align-all-tags)
  (load! +orgmode-mediawiki)
)



(add-hook 'org-load-hook #'my/org-mode-hook)


;; setup deft
(def-package! deft :after org-mode :demand t :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))

;;;;;;;;;;;;;
;; NEOTREE ;;
;;;;;;;;;;;;;

;; change icon size. Possibly commented out for being weird / inefficient.
(defun text-scale-twice ()(interactive)(progn(text-scale-adjust 0)(text-scale-decrease 0)))
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))
;; window config
(setq neo-window-width 30)
(setq neo-window-fixed-size nil)


;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

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

 ;; ORG MODE
 org-refile-targets (quote (("notes.org" :maxlevel . 1) ("learning.org" :maxlevel . 3)))
 org-outline-path-complete-in-steps nil ; Refile in a single go
 org-refile-use-outline-path t ; Show full paths for refiling
 org-tags-column 80
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL VARS SETQ STUFF ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ivy-re-builders-alist '((t . ivy--regex-fuzzy))                                      ;; Make ivy a fuzzy searcher.
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 )

(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2)


 ;;;;;;;;;;;;;;;;;;;
 ;; MODES + HOOKS ;;
 ;;;;;;;;;;;;;;;;;;;

(defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'before-save-hook 'whitespace-cleanup)

