;; lang/org
;; markdown style headings.
(after! org-bullets (setq org-bullets-bullet-list '("#")))
(setq +org-dir "~/Dropbox/notes/")
(setq org-default-notes-file "~/Desktop/notes/organizer.org")

;; UI
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic)

;; PACKAGES AND MODES ----------------------------------------------------------

;; (def-package! flx :demand t) ;; this is for fuzzy searching in ivy i think.

(defun +racket/repl ()
  (interactive)
  (run-racket)
  (current-buffer))

(defun pseudo-racket-mode ()
  (set-repl-handler! 'scheme-mode #'+racket/repl)
  (set-popup-rule! "^\\*Racket REPL" :quit nil :select nil)
  (set-eval-handler! 'scheme-mode #'geiser-eval-definition))

(def-package! geiser
  :config
  (setq geiser-active-implementations '(racket))
  (setq geiser-racket-binary "/Applications/Racket v6.12/bin/racket"))

(def-package! prettier-js :mode "\\.js$" :config)
(def-package! js-import :commands js-import :config)
(def-package! writeroom-mode :commands writeroom-mode :config)

(def-package! deft
  :demand t
  :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :demand t
  :config
  (setq avy-all-windows t avy-background t))

;; note to self: C-x C-f for company file auto complete
(require 'company)

(push '("\\.js\\'"   . rjsx-mode)   auto-mode-alist)
(push '("\\.css\\'"  . web-mode)    auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)   auto-mode-alist)
(push '("\\.rkt\\'" .  scheme-mode) auto-mode-alist)

;; -- PACKAGE CHANGES ----------------------------------------------------------

(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; DEFAULTS --------------------------------------------------------------------

;; menu bar thing.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq-default
 ;; GENERAL STUFF
 line-spacing 0.1
 tab-width 2
 indent-tab-mode nil
 which-key-idle-delay 0.2
 evil-ex-search-case 'sensitive
 auto-window-vscroll nil ;; apparently this slows down emacs if true

 ;;;; WEB JS AND WHATEVER STUFF
 js2-bounce-indent-p nil
 js2-highlight-level 3
 js2-basic-offset 2
 js-indent-level 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-style-padding 2
 web-mode-script-padding 2
 css-indent-offset 2

 ;; PLUGINS
 ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy)) ; Make ivy a fuzzy searcher.
 counsel-rg-base-command "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 company-idle-delay 0.2
 company-minimum-prefix-length 3
 neo-window-width 30
 neo-window-fixed-size nil
 deft-auto-save-interval 20
 avy-all-windows 'all-frames

 ;; ORG MODE
 org-refile-targets (quote (("notes.org" :maxlevel . 1) ("todo.org" :maxlevel . 1)))
 org-outline-path-complete-in-steps nil ; Refile in a single go
 org-refile-use-outline-path t          ; Show full paths for refiling
 org-log-done 'time
 org-agenda-files '("~/Dropbox/notes" "~/work/toda/notes")
 org-tags-column 80)


 ;; MODES + HOOKS + FUNCTIONS --------------------------------------------------


(defun tees/home ()
  "Make things ok on shitty monitor"
  (interactive)
  (set-frame-font "IBM Plex Mono-14" nil t))


(defun tees/nomad ()
  "me and my paltop."
  (interactive)
  (set-frame-font "IBM Plex Mono-12" nil t))


(defun tees/init-diff ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files
   "~/.config/doom/init.el"
   "~/.emacs.d/init.example.el"))

(defun cljfmt ()
  "require cljfmt binary from graalvm. Doesn't really work that well..."
  (interactive)
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (exec-path-from-shell-initialize)
    (shell-command-to-string (format "cljfmt %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))




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

(defun tees/clip-file ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (x-select-text filename))))

(defun neotree-text-size ()
  "Change neotree textsize."
  (interactive)
  (progn (text-scale-adjust 0)(text-scale-decrease 0)))

(defun tees/org-mode-hook ()
  "Setup my org mode to do it's magic. Aligns tags, change heading sizes / backgrounds."
  (interactive)
  (setq org-tags-column 80)
  (org-align-all-tags)
  (dolist (level '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6))
    (set-face-attribute level nil :height 1.0 :background nil)))

(defun tees/web-mode-hook ()
  "Hooks for Web mode."
  (interactive)
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset    2
   web-mode-code-indent-offset   2
   web-mode-style-padding        2
   web-mode-script-padding       2))

(defun tees/clojure-indent ()
  (interactive)
  (setq clojure-defun-style-default-indent
        (if (eq t clojure-defun-style-default-indent) nil 't))
  (message "Clojure default indent set to: %s"
           (if (eq t clojure-defun-style-default-indent) "true" "false")))


(add-hook 'web-mode-hook     'tees/web-mode-hook)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'org-load-hook     'tees/org-mode-hook)
(add-hook 'scheme-mode-hook  'pseudo-racket-mode)
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'neotree-text-size)))


;;; BINDINGS -------------------------------------------------------------------

(map!
 ;; <GLOBAL>
 ;; there appears to be nothing here...

 ;; <LEADER>
 (:leader
   :desc "toggle last buffer"     :nv "TAB" #'evil-switch-to-windows-last-buffer
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
     :desc "Imenu anywhere"       :n "f" #'ivy-imenu-anywhere
     :desc "Imenu here"           :n "h" #'counsel-imenu
     :desc "iMenu"                :n "i" #'imenu-list

     :desc "Jump back"            :n "b" #'dumb-jump-back
     :desc "Google"               :n "g" #'+jump/online)

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
     :desc "Neotree"              :n  "N" #'neotree-find
     :desc "Treemacs"             :n  "n" #'treemacs
     :desc "APP: Deft"            :n  "D" #'deft)

   (:desc "eval" :prefix "e"
     :desc "Eval buffer"          :n "b" #'eval-buffer
     :desc "Eval region"          :n "r" #'eval-region)

   (:desc "util" :prefix "U"
     :desc "Copy buffer path"     :n "c" #'tees/clip-file
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


;; stolen from : https://emacs.stackexchange.com/questions/32178/how-to-create-table-of-time-distribution-by-tags-in-org-mode/32182
;; for clocking time by task.

(require 'org-table)
(require 'org-clock)

(defun clocktable-by-tag/shift-cell (n)
  (let ((str ""))
    (dotimes (i n)
      (setq str (concat str "| ")))
    str))

(defun clocktable-by-tag/insert-tag (params)
  (let ((tag (plist-get params :tags)))
    (insert "|--\n")
    (insert (format "| %s | *Tag time* |\n" tag))
    (let ((total 0))
  (mapcar
       (lambda (file)
     (let ((clock-data (with-current-buffer (find-file-noselect file)
                 (org-clock-get-table-data (buffer-name) params))))
       (when (> (nth 1 clock-data) 0)
         (setq total (+ total (nth 1 clock-data)))
         (insert (format "| | File *%s* | %.2f |\n"
                 (file-name-nondirectory file)
                 (/ (nth 1 clock-data) 60.0)))
         (dolist (entry (nth 2 clock-data))
           (insert (format "| | . %s%s | %s %.2f |\n"
                   (org-clocktable-indent-string (nth 0 entry))
                   (nth 1 entry)
                   (clocktable-by-tag/shift-cell (nth 0 entry))
                   (/ (nth 3 entry) 60.0)))))))
       (org-agenda-files))
      (save-excursion
    (re-search-backward "*Tag time*")
    (org-table-next-field)
    (org-table-blank-field)
    (insert (format "*%.2f*" (/ total 60.0)))))
    (org-table-align)))

(defun org-dblock-write:clocktable-by-tag (params)
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|     |          | <r>  |\n")
  (let ((tags (plist-get params :tags)))
    (mapcar (lambda (tag)
          (setq params (plist-put params :tags tag))
          (clocktable-by-tag/insert-tag params))
        tags)))

(provide 'clocktable-by-tag)
