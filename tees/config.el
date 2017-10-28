;;; private/tees/config.el -*- lexical-binding: t; -*-

;; TODO: add cmd+z / cmd+shift+z undo redo
;; TODO: add layers to the windows for nested perspectives.

;; MODES
(def-package! prettier-js :mode "\\.js$" :config)
(def-package! js-import :commands js-import :config)
(def-package! rjsx-mode :commands rjsx-mode :config)
(def-package! eyebrowse :demand t)

(eyebrowse-mode)
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
     :desc "Flycheck"             :n "x" #'flycheck-mode
     :desc "Line-wrap"            :n "l" #'toggle-truncate-lines
     :desc "Line-numbers"         :n "n" #'doom/toggle-line-numbers
     :desc "Load theme"           :n "s" #'load-theme)

   (:desc "code" :prefix "c"
     :desc "Comment line"         :n "l" #'comment-line)

   (:desc "jump" :prefix "j"
     :desc "Jump to Char"         :n "j" #'avy-goto-char
     :desc "Google"               :n "g" #'+jump/online)

   (:desc "buffer" :prefix "b"
     :desc "kill buffer"          :n "d" #'doom/kill-this-buffer)

   (:desc "file" :prefix "f"
     :desc "Find File"            :n "f" #'counsel-find-file
     :desc "Save buffer"          :n "s" #'save-buffer)

   (:desc "project" :prefix "p"
     :desc "Find File in Project" :n "f" #'projectile-find-file)

   (:desc "window" :prefix "w"
     :desc "close window"         :n "d" #'+workspace/close-window-or-workspace
     :desc "split vert"           :n "-" #'split-window-vertically
     :desc "split horiz"          :n "/" #'split-window-horizontally)

   (:desc "workspace" :prefix "l"
     :desc "Switch to"            :n "f" #'+workspace/switch-to
     :desc "Save as"              :n "s" #'+workspace/save
     :desc "Save session"         :n "S" #'+workspace/save
     :desc "Load"                 :n "l" #'+workspace/load
     :desc "Load Session"         :n "L" #'+workspace/load-session
     :desc "New"                  :n "n" #'+workspace/new
     :desc "Rename"               :n "r" #'+workspace/rename)

   (:desc "eyebrowse" :prefix "k"
     :desc "Switch to"            :n "k" #'eyebrowse-switch-to-window-config
     :desc "Delete"               :n "d" #'eyebrowse-close-window-config
     :desc "New"                  :n "n" #'eyebrowse-create-window-config
     :desc "Rename"               :n "r" #'eyebrowse-rename-window-config)

   (:desc "git" :prefix "g"
     :desc "Git status"           :n  "s" #'magit-status
     :desc "Git stage hunk"       :n  "S" #'git-gutter:stage-hunk
     :desc "List gists"           :n  "g" #'+gist:list)

   (:desc "eval" :prefix "e"
     :desc "Eval buffer"          :n "b" #'eval-buffer
     :desc "Eval region"          :n "r" #'eval-region)
))
