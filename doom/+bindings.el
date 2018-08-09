;;; ~/Development/dotfiles/doom/+bindings.el -*- lexical-binding: t; -*-


(map!
 ;; <GLOBAL>
 ;; there appears to be nothing here...

 ;; <LEADER>
 (:leader
   :desc "toggle last buffer"     :n [tab] #'evil-switch-to-windows-last-buffer
;   :desc "project-search"         :nv "/" #'counsel-rg

   (:desc "toggle" :prefix "t"
     :desc "Flycheck"             :n "x" #'flycheck-mode
     :desc "Line-wrap"            :n "l" #'toggle-truncate-lines
     :desc "Line-numbers"         :n "n" #'doom/toggle-line-numbers
     :desc "Load theme"           :n "s" #'load-theme)

   (:desc "code" :prefix "c"
     :desc "Comment dwim"         :n "L" #'comment-dwim
     :desc "Comment line"         :n "l" #'comment-line
     :desc "Comment region"       :n "v" #'comment-region
     )

   (:desc "jump" :prefix "j"
     :desc "Jump to Char"         :n "a" #'avy-goto-char
     :desc "Jump to Line"         :n "l" #'avy-goto-line
     :desc "Jump to..."           :n "j" #'dumb-jump-go
     :desc "Set Mark"             :n "m" #'evil-set-marker
     :desc "Goto Mark"            :n "g" #'evil-goto-mark
     :desc "Imenu anywhere"       :n "f" #'ivy-imenu-anywhere
     :desc "Imenu here"           :n "h" #'counsel-imenu
     :desc "iMenu"                :n "i" #'imenu-list
     :desc "Jump back"            :n "b" #'dumb-jump-back
     )

   (:desc "buffer" :prefix "b"
     :desc "kill buffer"          :n "d" #'kill-this-buffer
     :desc "kill buffer"          :n "k" #'kill-this-buffer)

   (:desc "file" :prefix "f"
     :desc "Find File"            :n "f" #'counsel-find-file
     :desc "Save buffer"          :n "s" #'save-buffer)

   (:desc "project" :prefix "p"
     :desc "Find File in Project" :n "f" #'projectile-find-file)

   (:desc "insert" :prefix "i"
     :desc "kill-ring"            :n "y" #'counsel-yank-pop)

   (:desc "window" :prefix "w"
     :desc "close window"         :n "d" #'+workspace/close-window-or-workspace
     :desc "split vert"           :n "-" #'split-window-vertically
     :desc "max-buffer"           :n "m" #'tees/max-buffer
     :desc "split horiz"          :n "/" #'split-window-horizontally)

   (:desc "workspace" :prefix "l"
     :desc "Switch to"            :n "f" #'+workspace/switch-to
     :desc "Save as"              :n "s" #'+workspace/save
     :desc "Save session"         :n "S" #'+workspace/save
     :desc "Load"                 :n "l" #'+workspace/load
     :desc "Load Session"         :n "L" #'+workspace/load-session
     :desc "New"                  :n "N" #'+workspace/new
     :desc "New+name"             :n "n" (lambda! () (+workspace/new (read-string "Enter workspace name: ")))
     :desc "Copy"                 :n "c" #'persp-copy
     :desc "Rename"               :n "r" #'+workspace/rename)

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
     :desc "Neotree"              :n  "n" #'neotree-find
     ;; :desc "Treemacs"             :n  "n" #'treemacs-find-file
     :desc "APP: Deft"            :n  "D" #'deft)

   (:desc "eval" :prefix "e"
     :desc "Eval buffer"          :n "b" #'eval-buffer
     :desc "Eval region"          :n "r" #'eval-region)

   (:desc "util" :prefix "z"
     :desc "Copy buffer path"     :n "c" #'tees/clip-file
     :desc "make last"            :n "z" #'makefile-executor-execute-last
     :desc "make choice..."       :n "x" #'makefile-executor-execute-project-target
     )

   (:desc "+search" :prefix "/"
     :desc "RG everywhere" :n "/" #'counsel-rg)

   (:desc "lisp" :prefix "k"
     :desc "sp-copy"              :n "c" #'sp-copy-sexp
     :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
     :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
     :desc "sp-up"                :n "u" #'sp-up-sexp
     :desc "sp-down"              :n "d" #'sp-down-sexp
     :desc "sp-kill"              :n "k" #'sp-kill-sexp
     :desc "sp-next"              :n "l" #'sp-next-sexp
     :desc "sp-prev"              :n "h" #'sp-previous-sexp)

   (:desc "cursors" :prefix "d"
     :desc "Make cursors"         :n "d" #'evil-mc-make-and-goto-next-match
     :desc "Remove cursors"       :n "c" #'evil-mc-undo-all-cursors))


 ;; -- LOCAL LEADERS --

 ;; (:map org-mode-map
 ;;   :localleader
 ;;   :desc "Insert heading above"          :n "N" #'org-insert-heading
 ;;   :desc "Insert heading below"          :n "n" #'org-insert-heading-after-current
 ;;   :desc "Insert subheading"             :n "s" #'org-insert-subheading
 ;;   :desc "Todo Hydra"                    :n "t" #'org-todo
 ;;   :desc "Clock in"                      :n "i" #'org-clock-in
 ;;   :desc "Clock out"                     :n "o" #'org-clock-out
 ;;   :desc "Jump"                          :n "j" #'counsel-org-goto)

 )
