;; lang/org
;; markdown style headings.
(after! org-bullets (setq org-bullets-bullet-list '("#")))
(setq +org-dir "~/Dropbox/notes/")
(setq org-default-notes-file "~/Desktop/notes/organizer.org")


;; UI
(set-face-attribute 'default nil :font "IBM Plex Mono-12")
(set-face-attribute 'font-lock-comment-face nil  :slant 'italic)

(defun tees/home ()
  "Make things ok on shitty monitor"
  (interactive)
  (set-frame-font "IBM Plex Mono-14" nil t))


(defun tees/nomad ()
  "me and my paltop."
  (interactive)
  (set-frame-font "IBM Plex Mono-12" nil t))


;; PACKAGES AND MODES

(def-package! flx :demand t) ;; this is for fuzzy searching in ivy i think.
(def-package! prettier-js    :mode "\\.js$"           :config)
(def-package! js-import      :commands js-import      :config)
(def-package! writeroom-mode :commands writeroom-mode :config)
(def-package! parinfer :commands parinfer-mode)
(def-package! deft :demand t :config
  (setq deft-extensions '("txt" "tex" "org" "md"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory "~/Dropbox/notes/"))
(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :demand t
  :config
  (setq avy-all-windows t avy-background t))

(require 'company)

(push '("\\.js\\'"   . rjsx-mode)   auto-mode-alist)
(push '("\\.css\\'"  . web-mode)    auto-mode-alist)
(push '("\\.sass\\'" . sass-mode)   auto-mode-alist)


;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;              pretty-parens  ; different paren styles for different modes.
;;              evil           ; If you use Evil.
;;                                         ; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;              paredit        ; Introduce some paredit commands.
;;              smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;              smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook     #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook  #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook      #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook        #'parinfer-mode)))


;; DEFAULTS

(setq-default
 ;; GENERAL STUFF
 line-spacing 0.1
 tab-width 2
 indent-tab-mode nil
 which-key-idle-delay 0.3
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
 org-tags-column 80)


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; MODES + HOOKS + FUNCTIONS ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2))


(add-hook 'web-mode-hook  'tees/web-mode-hook)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'org-load-hook 'tees/org-mode-hook)
(add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'neotree-text-size)))



;;;
;;;
;;; BINDINGS ---
;;;
;;;

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
     :desc "New"                  :n "n" #'+workspace/new
     :desc "Copy"                 :n "c" #'persp-copy
     :desc "Rename"               :n "r" #'+workspace/rename)

   (:desc "git" :prefix "g"
     :desc "Git status"           :n  "s" #'magit-status
     :desc "Git stage hunk"       :n  "S" #'git-gutter:stage-hunk
     :desc "Git checkout"         :n  "c" #'magit-branch-checkout
     :desc "merge-conflict"       :n  "m" #'+hydra-smerge/body
     :desc "List gists"           :n  "g" #'+gist:list)

   (:desc "open" :prefix "o"
     :desc "Eshell"               :n  "e" #'+eshell/open-popup
     :desc "Neotree"              :n  "n" #'neotree-find
     :desc "APP: Deft"            :n  "D" #'deft)

   (:desc "eval" :prefix "e"
     :desc "Eval buffer"          :n "b" #'eval-buffer
     :desc "Eval region"          :n "r" #'eval-region)

   (:desc "util" :prefix "U"
     :desc "Copy buffer path"     :n "c" #'tees/clip-file
     )

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
     :desc "Remove cursors"       :n "c" #'evil-mc-undo-all-cursors)))
