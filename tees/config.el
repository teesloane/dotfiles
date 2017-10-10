;;; private/tees/config.el -*- lexical-binding: t; -*-

;; TODO: neotree adjustable
;; TODO: add parinfer
;; TODO: add cmd+z / cmd+shift+z undo redo
;; TODO: disable goggles
;; TODO: ctrl + tab for moving persp
;; TODO: add layers to the windows for nested perspectives.

;; MODES
(push '("\\.js\\'" . rjsx-mode) auto-mode-alist)

;; This might be slowing boot ... should make this happen on hooks...
(def-package! rjsx-mode        :demand t :config)
;; (def-package! doom-themes      :demand t :config) ;; I think this is already installed
(def-package! prettier-js      :demand t :config)
(def-package! browse-at-remote :demand t :config)
(def-package! yaml-mode        :demand t :config)
(def-package! markdown-mode    :demand t :config)


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
    web-mode-markup-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-style-padding 2
    web-mode-script-padding 2

    ;; PLUGINS
    evil-goggles-duration 0.100 ;; default is 0.200
    avy-all-windows t
)

;; KEYBINDINGS
(map!
 ;; --- <GLOBAL> -------------------------------------

 ;; --- <LEADER> -------------------------------------
 (:leader
   :desc "toggle last buffer"     :nv "TAB" #'evil-switch-to-windows-last-buffer
   :desc "project-search"         :nv "/" #'counsel-rg

   (:desc "toggle" :prefix "t"
     :desc "Flycheck"             :n "n" #'flycheck-mode
     :desc "Load theme"           :n "s" #'load-theme)

   (:desc "jump" :prefix "j"
     :desc "Jump to Char"         :n "j" #'avy-goto-char)

   (:desc "buffer" :prefix "b"
     :desc "kill buffer"          :n "d" #'doom/kill-this-buffer)

   (:desc "file" :prefix "f"
     :desc "Save buffer"          :n "s" #'save-buffer)

   (:desc "project" :prefix "p"
     :desc "Find File in Project" :n "f" #'projectile-find-file)

   (:desc "window" :prefix "w"
     :desc "close window"         :n "d" #'+workspace/close-window-or-workspace
     :desc "split vert"           :n "-" #'split-window-vertically
     :desc "split horiz"          :n "/" #'split-window-horizontally)

   (:desc "git" :prefix "g"
     :desc "Git status"           :n  "s" #'magit-status
     :desc "Git stage hunk"       :n  "S" #'git-gutter:stage-hunk
     :desc "List gists"           :n  "g" #'+gist:list
     :desc "Open in remote"       :n  "o" #'browse-at-remote)

   (:desc "eval" :prefix "e"
     :desc "Eval region"          :n "r" #'eval-region)
))
