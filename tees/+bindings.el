;;; +bindings.el --- description -*- lexical-binding: t; -*-
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
     :desc "Jump to Char"         :n "a" #'avy-goto-char
     :desc "Jump to..."           :n "j" #'dumb-jump-go
     :desc "Imenu anywhere"       :n "f" #'ivy-imenu-anywhere
     :desc "Imenu here"           :n "h" #'counsel-imenu
     :desc "iMenu"                :n "i" #'imenu-list

     :desc "Jump back"            :n "b" #'dumb-jump-back
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
     :desc "Copy"                 :n "c" #'persp-copy
     :desc "Rename"               :n "r" #'+workspace/rename)

   (:desc "eyebrowse" :prefix "k"
     :desc "Switch to"            :n "k" #'eyebrowse-switch-to-window-config
     :desc "Delete"               :n "d" #'eyebrowse-close-window-config
     :desc "New"                  :n "n" #'eyebrowse-create-window-config
     :desc "Rename"               :n "r" #'eyebrowse-rename-window-config)

   (:desc "git" :prefix "g"
     :desc "Git status"           :n  "s" #'magit-status
     :desc "Git stage hunk"       :n  "S" #'git-gutter:stage-hunk
     :desc "Git checkout"         :n  "c" #'magit-branch-checkout
     :desc "merge-conflict"       :n  "m" #'+hydra-smerge/body
     :desc "List gists"           :n  "g" #'+gist:list)

   (:desc "open" :prefix "o"
     :desc "Eshell"               :n  "e" #'+eshell/open-popup)

   (:desc "eval" :prefix "e"
     :desc "Eval buffer"          :n "b" #'eval-buffer
     :desc "Eval region"          :n "r" #'eval-region)

   (:desc "cursors" :prefix "d"
     :desc "Make cursors"         :n "d" #'evil-mc-make-and-goto-next-match
     :desc "Remove cursors"       :n "c" #'evil-mc-undo-all-cursors
     )

))


(provide '+bindings)
;;; +bindings.el ends here
