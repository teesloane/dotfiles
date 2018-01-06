;;; private/tees/config.el -*- lexical-binding: t; -*-

(load! +bindings)

;; some fonts
(set-face-attribute 'default nil :font "IBM Plex Mono-12")
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic )


;; MODES
(def-package! flx :demand t) ;; this is for fuzzy searching in ivy i think.
(def-package! prettier-js    :mode "\\.js$"           :config)
(def-package! js-import      :commands js-import      :config)
(def-package! writeroom-mode :commands writeroom-mode :config)
(def-package! drag-stuff :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))
(def-package! deft :demand t :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))

(push '("\\.js\\'"   . rjsx-mode)   auto-mode-alist)
(push '("\\.css\\'"  . web-mode)    auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)   auto-mode-alist)

 ;;;;;;;;;;;;;;;;;;;;
 ;; SOME FUNCTIONS ;;
 ;;;;;;;;;;;;;;;;;;;;

;; Align funcs ripped from http://pragmaticemacs.com/emacs/aligning-text/

(defun tees/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun tees/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; MODES + HOOKS + FUNCTIONS ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neotree-text-size ()
  "Change neotree textsize."
  (interactive)
  (progn (text-scale-adjust 0)(text-scale-decrease 0)))

(defun my/org-mode-hook ()
  "Setup my org mode to do it's magic. Aligns tags, change heading sizes / backgrounds."
  (load! +orgmode-mediawiki)
  (org-align-all-tags)
  (dolist (level '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6))
    (set-face-attribute level nil :height 1.0 :background nil)))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2))


(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'org-load-hook #'my/org-mode-hook)
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'neotree-text-size)))
