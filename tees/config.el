;;; private/tees/config.el -*- lexical-binding: t; -*-

;; TODO: avy jump between buffers
;; TODO: neotree adjustable or add treemacs.
;; TODO: add parinfer
;; TODO: add cmd+z / cmd+shift+z undo redo
;; TODO: ctrl + tab for moving persp
;; TODO: add layers to the windows for nested perspectives.

;; PACKAGES
(def-package! rjsx-mode        :demand t :config)
(def-package! emojify          :demand t :config)
(def-package! doom-themes      :demand t :config)
(def-package! prettier-js      :demand t :config)
(def-package! browse-at-remote :demand t :config)
(def-package! yaml-mode        :demand t :config)
(def-package! markdown-mode    :demand t :config)

;; MODES
(push '("\\.js\\'" . rjsx-mode) auto-mode-alist)

;; DEFAULTS
(setq-default
    ;; GENERAL STUFF
    line-spacing 0.8
    tab-width 2
    which-key-idle-delay 0.3
    initial-major-mode 'org-mode      ;; set scratch buffer to org

    ;; JAVASCRIPT STUFF
    js2-bounce-indent-p nil
    js2-highlight-level 3
    js2-basic-offset 2
    js-indent-level 2

    ;; PLUGINS
    evil-goggles-duration 0.100 ;; default is 0.200
)

;; KEYBINDINGS
(map!
 (:leader
   :desc "toggle last buffer"    :nv "TAB" #'evil-switch-to-windows-last-buffer
   :desc "project-search"        :nv "/" #'counsel-rg

   (:desc "toggle" :prefix "t"
     :desc "Load theme"               :n "s" #'load-theme)

   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git stage hunk"    :n  "S" #'git-gutter:stage-hunk
     :desc "List gists"        :n  "g" #'+gist:list
     :desc "Open in remote"    :n  "o" #'browse-at-remote)

   (:desc "eval" :prefix "e"
     :desc "Eval region"               :n "r" #'eval-region
     )
))
