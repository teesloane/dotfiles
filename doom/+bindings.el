;;; ~/Development/dotfiles/doom/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; <GLOBAL>
 :n "C-h" #'evil-window-left
 :n "C-l" #'evil-window-right
 :n "C-j" #'evil-window-down
 :n "C-k" #'evil-window-up

 ;; Switching workspaces / Workspace commans
 "s-1"   (λ! (+workspace/switch-to 0))
 "s-2"   (λ! (+workspace/switch-to 1))
 "s-3"   (λ! (+workspace/switch-to 2))
 "s-4"   (λ! (+workspace/switch-to 3))
 "s-5"   (λ! (+workspace/switch-to 4))
 "s-6"   (λ! (+workspace/switch-to 5))
 "s-7"   (λ! (+workspace/switch-to 6))
 "s-8"   (λ! (+workspace/switch-to 7))
 "s-9"   (λ! (+workspace/switch-to 8))
 :n "s-T" #'+workspace/display
 :n "s-t" (lambda! () (+workspace/new (read-string "Enter workspace name: ")))

 ;; <LEADER>
 (:leader
   ;; :desc "toggle last buffer"     :n [tab] #'evil-switch-to-windows-last-buffer
   ;; :desc "project-search"         :nv "/" #'counsel-rg

   (:desc "toggle" :prefix "t"
     :desc "Flycheck"             :n "x" #'flycheck-mode
     :desc "Line-wrap"            :n "l" #'toggle-truncate-lines
     :desc "Line-numbers"         :n "n" #'doom/toggle-line-numbers
     :desc "Load theme"           :n "s" #'load-theme)

   (:desc "search" :prefix "/"
     :desc "RG"           :nv "/" #'counsel-rg)

   (:desc "code" :prefix "c"
     :desc "Comment dwim"         :n "L" #'comment-dwim
     :desc "Comment line"         :n "l" #'comment-line
     :desc "Comment region"       :n "v" #'comment-region)


   (:desc "jump" :prefix "j"
     :desc "Jump to Char"         :n "a" #'avy-goto-char
     :desc "Jump to Line"         :n "l" #'avy-goto-line
     :desc "Jump to..."           :n "j" #'dumb-jump-go
     :desc "Set Mark"             :n "m" #'evil-set-marker
     :desc "Goto Mark"            :n "g" #'evil-goto-mark
     :desc "Imenu anywhere"       :n "f" #'ivy-imenu-anywhere
     :desc "Imenu here"           :n "h" #'counsel-imenu
     :desc "iMenu"                :n "i" #'imenu-list
     :desc "Jump back"            :n "b" #'dumb-jump-back)

   (:desc "insert" :prefix "i"
     :desc "kill-ring"            :n "y" #'counsel-yank-pop)

   (:desc "window" :prefix "w"
     :desc "close window"         :n "d" #'+workspace/close-window-or-workspace
     :desc "split vert"           :n "-" #'split-window-vertically
     :desc "max-buffer"           :n "m" #'tees/max-buffer
     :desc "previous buff"        :n "w" #'evil-switch-to-windows-last-buffer
     :desc "split horiz"          :n "/" #'split-window-horizontally)

   (:desc "workspace" :prefix "l"
     :desc "New+name"                  "n"   (lambda! () (+workspace/new (read-string "Enter workspace name: ")))
     :desc "Copy"                      "c"   #'persp-copy
     :desc "Rename"                    "r"   #'+workspace/rename
     :desc "Display tab bar"           "TAB" #'+workspace/display
     :desc "New workspace"             "n"   #'+workspace/new
     :desc "Load workspace from file"  "l"   #'+workspace/load
     :desc "Load a past session"       "L"   #'+workspace/load-session
     :desc "Save workspace to file"    "s"   #'+workspace/save
     :desc "Autosave current session"  "S"   #'+workspace/save-session
     :desc "Switch workspace"          "."   #'+workspace/switch-to
     :desc "Delete session"            "x"   #'+workspace/kill-session
     :desc "Delete this workspace"     "d"   #'+workspace/delete
     :desc "Rename workspace"          "r"   #'+workspace/rename
     :desc "Restore last session"      "R"   #'+workspace/load-last-session
     :desc "Next workspace"            "]"   #'+workspace/switch-right
     :desc "Previous workspace"        "["   #'+workspace/switch-left
     :desc "Switch to 1st workspace"   "1"   (λ! (+workspace/switch-to 0))
     :desc "Switch to 2nd workspace"   "2"   (λ! (+workspace/switch-to 1))
     :desc "Switch to 3rd workspace"   "3"   (λ! (+workspace/switch-to 2))
     :desc "Switch to 4th workspace"   "4"   (λ! (+workspace/switch-to 3))
     :desc "Switch to 5th workspace"   "5"   (λ! (+workspace/switch-to 4))
     :desc "Switch to 6th workspace"   "6"   (λ! (+workspace/switch-to 5))
     :desc "Switch to 7th workspace"   "7"   (λ! (+workspace/switch-to 6))
     :desc "Switch to 8th workspace"   "8"   (λ! (+workspace/switch-to 7))
     :desc "Switch to 9th workspace"   "9"   (λ! (+workspace/switch-to 8))
     :desc "Switch to last workspace"  "0"   #'+workspace/switch-to-last)


   (:desc "git" :prefix "g"
     :desc "Status"                :n  "s" #'magit-status
     :desc "Git stage hunk"       :n  "S" #'git-gutter:stage-hunk
     :desc "Checkout"             :n  "c" #'magit-branch-checkout
     :desc "Branch"               :n  "b" #'magit-branch-popup
     :desc "Blame"                :n  "B" #'magit-blame
     :desc "Issues"               :n  "I" #'magithub-issue-view
     :desc "merge-conflict"       :n  "m" #'+hydra-smerge/body
     :desc "Gists"                :n  "g" #'+gist:list)

   (:desc "open" :prefix "o"
     :desc "Eshell"               :n  "e" #'+eshell/open-popup
     ;; :desc "Neotree"              :n  "n" #'neotree-find
     :desc "Treemacs"             :n  "n" #'+treemacs/toggle
     :desc "APP: Deft"            :n  "D" #'deft)


   ;; (:desc "util" :prefix "z"
   ;;   :desc "Copy buffer path"     :n "c" #'tees/clip-file
   ;;   :desc "make last"            :n "z" #'makefile-executor-execute-last
   ;;   :desc "make choice..."       :n "x" #'makefile-executor-execute-project-target)

   (:desc "lisp" :prefix "k"
     :desc "sp-copy"              :n "c" #'sp-copy-sexp
     :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
     :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
     :desc "sp-up"                :n "u" #'sp-up-sexp
     :desc "sp-down"              :n "d" #'sp-down-sexp
     :desc "sp-kill"              :n "k" #'sp-kill-sexp
     :desc "sp-next"              :n "l" #'sp-next-sexp
     :desc "sp-prev"              :n "h" #'sp-previous-sexp)))
