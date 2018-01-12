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
  (drag-stuff-define-keys))

(def-package! deft :demand t :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :config
  (setq avy-all-windows t
        avy-background t))

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


(defun tees/max-buffer ()
  "Current buffer becomes full width"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; some cool defaults.

(setq-default
 ;; GENERAL STUFF
 line-spacing 0.2
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
 ivy-re-builders-alist '((t . ivy--regex-fuzzy))                                      ;; Make ivy a fuzzy searcher.
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 company-idle-delay 0.2
 company-minimum-prefix-length 2
 neo-window-width 30
 neo-window-fixed-size nil
 deft-auto-save-interval 20
 avy-all-windows 'all-frames

 ;; ORG MODE
 org-refile-targets (quote (("notes.org" :maxlevel . 1) ("learning.org" :maxlevel . 3)))
 org-outline-path-complete-in-steps nil ; Refile in a single go
 org-refile-use-outline-path t          ; Show full paths for refiling
 org-log-done 'time
 org-agenda-files '("~/Dropbox/notes" "~/work/toda/notes")
 org-tags-column 80
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; MODES + HOOKS + FUNCTIONS ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neotree-text-size ()
  "Change neotree textsize."
  (interactive)
  (progn (text-scale-adjust 0)(text-scale-decrease 0)))

(defun tees/org-mode-hook ()
  "Setup my org mode to do it's magic. Aligns tags, change heading sizes / backgrounds."
  (interactive)
  (drag-stuff-global-mode nil)
  (dolist (level '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6))
    (set-face-attribute level nil :height 1.0 :background nil)))

(defun shadow-cider (build-id)
  "boot shadow-cljs magically, avoiding prompts and stuff;
  should run from project file so cider boots into the project.
  NOTE: you have to press `return' once inside the repl to trigger cider
  recognizing you are now in a cljs repl."

  (interactive "sEnter build id: ")
  (cider-connect "localhost" "8202" (buffer-file-name))
  (cider-read-and-eval (format "(shadow.cljs.devtools.api/nrepl-select :%s)" build-id))
  )

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
(add-hook 'org-load-hook 'tees/org-mode-hook)
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'neotree-text-size)))
