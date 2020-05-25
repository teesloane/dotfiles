;; Startup funcs


;;; Startup Calls --

(menu-bar-mode t)
(fringe-mode 0)

;;; Variable overrides --

(setq-default
 _wiki-path                   "~/Dropbox/wiki/"
 avy-all-windows              'all-frames
 deft-directory               _wiki-path
 projectile-project-search-path '("~/Projects" "~/Development")
 time-stamp-active             t
 time-stamp-format             "%04y-%02m-%02d %02H:%02M:%02S"
 which-key-idle-delay          0.3
 counsel-rg-base-command       "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 )

(setq org-babel-default-header-args '((:results . "replace") (:comments . "org")))

;;; Webby web web

(setq-default
 css-indent-offset             2
 js-indent-level               2
 js2-basic-offset              2
 js2-bounce-indent-p           nil
 js2-highlight-level           3
 web-mode-code-indent-offset   2
 web-mode-css-indent-offset    2
 web-mode-markup-indent-offset 2
 web-mode-script-padding       2
 web-mode-style-padding        2
 )

(after! centaur-tabs
  ;; (centaur-tabs-mode -1))
  (centaur-tabs-mode 1)
  (centaur-tabs-group-by-projectile-project)

  (setq centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "    ~    "
        centaur-tabs-left-edge-margin "     "
        centaur-tabs-close-button "   ×   "
        centaur-tabs-icon-v-adjust -0.03
        centaur-tabs-icon-scale-factor 0.6
        centaur-tabs-label-fixed-length 24
        centaur-tabs-style 'bar
        centaur-tabs-set-bar 'above)
  centaur-tabs-gray-out-icons 'buffer
  (centaur-tabs-change-fonts "IBM Plex Mono" 110))

;; Doom things

(setq-default
 global-whitespace-mode        0
 line-spacing                  2
 ;; doom-font                     (font-spec :family "JetBrains Mono" :size 13)
 doom-font                     (font-spec :family "Iosevka" :size 14 :weight 'regular)
 ;; doom-variable-pitch-font      (font-spec :family "JetBrains Mono" :size 12)
 doom-variable-pitch-font      (font-spec :family "IBM Plex Mono" :size 12)
 +zen-text-scale               0
 doom-theme                    'doom-opera
 )

(after! ivy-posframe
  (setq ivy-fixed-height-minibuffer nil
        ivy-posframe-border-width 10
        ivy-posframe-width 150
        ivy-posframe-parameters
        `((min-width . 150)
          (min-height . ,ivy-height))))

;; Best with custom Iosevka font. See, e.g., https://is.gd/L67AoR
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode clojure-mode
                                   latex-mode scheme-mode racket-mode ess-r-mode))

(setq highlight-indent-guides-responsive 'top
      highlight-indent-guides-delay 0)

;; Org and R additional symbols
;; hex code ▷ (9655), ◇ (9671), ▶ (9654), ƒ (402)
(setq +pretty-code-iosevka-font-ligatures
      (append +pretty-code-iosevka-font-ligatures
              '(("[ ]" .  "☐")
                ("[X]" . "☑" )
                ("[-]" . "❍" )
                ("%>%" . ?▷)
                ("%$%" . ?◇)
                ("%T>%" . ?▶)
                ("function" . ?ƒ))))

;; https://is.gd/3VuSXj
(defface org-checkbox-done-text
  '((t (:foreground "#5a637b")))
  "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords 'org-mode
                        '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                           1 'org-checkbox-done-text prepend))
                        'append)
;; (custom-set-faces '(org-checkbox ((t (:foreground nil :inherit org-todo)))))

;;; Magit --

;; Make magit render icons for common commit leaders (ex: "Fix:" becomes "")
(use-package! pretty-magit
  :init
  (pretty-magit "Feat" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Clean" ? '(:foreground "#B5E655" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Docs" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Test" ? '(:foreground "#4BB5C1" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Start" ? '(:foreground "#2ecc71" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Stop" ? '(:foreground "#e74c3c" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Refactor" ? '(:foreground "#9b59b6" :height 1.0 :family "FontAwesome"))
  (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
  (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t))

;;; Org Mode --

;; Org Directory

(setq
 org-agenda-files              '("~/Dropbox/wiki/inbox.org" "~/Dropbox/wiki/projects.org")
 org-default-notes-file        (concat _wiki-path "inbox.org")
 org-directory                 _wiki-path
 org-link-file-path-type       'relative
 )

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(after! org
  (setq
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets                     '((org-agenda-files :maxlevel . 3))
                                            ;(+org/opened-buffer-files :maxlevel . 2)) ;; < this was working ...
   org-refile-use-outline-path            'file ; Show/full/paths for refiling
   ))

;;; Org: general variable setting --

;; This is for getting refile targets from my open org files.
(defun my-org-files-list ()
  (delq nil
        (mapc (lambda (buffer)
                (buffer-file-name buffer))
              (org-buffer-list 'files t))))

(after! org
  ;; org variables not related to directories.
  (setq
   org-attach-id-dir                   "data/attachments/"
   org-bullets-bullet-list             '("⁖")
   org-superstar-headline-bullets-list '("⁖")
   org-log-done                        t
   org-log-into-drawer                 t
   org-outline-path-complete-in-steps  nil ; refile easy
   ))

(after! org-download
  (setq
   org-download-method                 'directory
   org-download-image-dir              "~/Dropbox/wiki/data/files/"))

(after! org (add-hook 'org-mode-hook 'turn-on-flyspell))

;; org - templates

(after! org
  (add-to-list 'org-capture-templates
               '("b" "New Book"
                 entry  ; type
                 (file "books.org") ; target
                 "* %^{Author} - %^{Title}
:PROPERTIES:
:author: %\\1
:title: %\\2
:pages: %^{Pages}
:page: 0
:date_started: %U
:date_completed:
:genre:
:type: %^{Type|Novel|Graphic Novel|Manga|Short Stories|Poetry|Other}
:rating: 0
:END:
"
                 :prepend t :kill-buffer t))

  (add-to-list 'org-capture-templates '("i" "Inbox" entry (file "inbox.org") "* %?\n%i\n" :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates '("l" "Log" entry (file+datetree "log.org.gpg") "**** %U %^{Title} %(org-set-tags-command) \n%?" :prepend t))
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file "inbox.org") "* TODO %?\n%i" :prepend t)))

;;; Org Agenda

(after! org
  (set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.5 :select t :ttl nil))

(after! org-agenda
  (org-super-agenda-mode)
  (use-package! org-super-agenda :commands (org-super-agenda-mode))

  (setq
   org-agenda-include-deadlines t
   org-agenda-start-with-log-mode t
   org-agenda-span 3
   org-agenda-block-separator ?-  ;; ?- is a "character" type. It evaluates to a num representing a char
   org-agenda-start-day "+0d"
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-skip-deadline-if-done t
   org-agenda-use-time-grid nil
   org-global-properties '(("Effort_ALL" . "0 0:10 0:20 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))
   org-agenda-tags-column 100
   org-agenda-compact-blocks nil)

  (setq org-agenda-exporter-settings
        '((ps-left-header (list 'org-agenda-write-buffer-name))
          (ps-right-header
           (list "/pagenumberstring load"
                 (lambda () (format-time-string "%d/%m/%Y"))))
          (ps-print-color-p 'black-white)
          (ps-font-size '(11 . 10))       ; Lanscape . Portrait
          (ps-top-margin 25)
          (ps-number-of-columns 1)
          (ps-landscape-mode t)
          (ps-left-margin 35)
          (ps-right-margin 30)))

  (setq org-agenda-custom-commands
        '(("a" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled nil
                            :deadline today
                            :discard (:anything t)
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Scheduled / Ongoing" :scheduled past)
                            (:name "Overdue" :deadline past)
                            (:name "Low effort" :effort< "1:00")
                            (:name "Recipes To Try" :tag "recipes")
                            (:name "Unscheduled/No Deadline" :scheduled nil :deadline nil  :order 8)
                            (:name "Other"   :order 8)))))))

          ("wt" "Work"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                        (org-super-agenda-groups
                         '((:name ""
                            :time-grid t
                            :scheduled today
                            :deadline today
                            :discard (:todo "WAIT" :todo "HOLD")
                            :order 1)))))

            (todo "" ((org-agenda-overriding-header "")
                      (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                      (org-super-agenda-groups
                       '(
                         (:name "IN PROGRESS" :todo  "PROJ" :todo "STRT")
                         (:name "BLOCKED" :todo  "WAIT" :todo "HOLD")
                         (:name "TASKS" :todo "TODO")
                         (:discard (:anything t))))))
            ;; Alternative to not getting the `(:tag "review")'
            (tags "review" ((org-agenda-overriding-header "")
                            (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                            (org-super-agenda-groups
                             '((:name "REVIEWS" :tag "review") ;; this isn't working.
                               (:discard (:anything t))))))))


          ;; show tasks that were "closed" over a one week span.
          ("ww" "Work Week Review"
           ((agenda "" ((org-agenda-span 'week)
                        (org-agenda-start-on-weekday 0)
                        (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                        (org-agenda-prefix-format "  %t %s")
                        (org-agenda-start-with-log-mode '(closed))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                        ;; this removes duplicate entries of tasks that were scheduled and marked done.
                        (org-super-agenda-groups
                         '((:name "" :time-grid t :discard (:anything t) :order 1)))))

            (todo "" ((org-agenda-overriding-header "")
                      (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                      (org-agenda-prefix-format "  %t %s")
                      (org-super-agenda-groups
                       '((:name "IN PROGRESS" :todo  "PROJ" :todo "STRT")
                         (:name "BLOCKED" :todo  "WAIT" :todo "HOLD")
                         (:name "TASKS" :todo "TODO")
                         (:discard (:anything t)))))))))))

(setq
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-long-break-sound-args "-volume 0.3"
 org-pomodoro-short-break-sound-args "-volume 0.3"
 )

;; Org general settings / ui

(after! org
  (setq
   line-spacing                           3
   org-cycle-separator-lines 2
   org-bullets-bullet-list                '("⁖")
   org-startup-truncated                  t
   org-ellipsis                          " ⋱ " ;; " • " ;; " ⇢ " ;; " ⋱ " ;;
   org-fontify-whole-heading-line         nil
   org-tags-column                        65
   org-image-actual-width                 400 ; set the width of inline images.
   org-habit-completed-glyph              ?✓
   org-habit-show-all-today               t
   org-habit-today-glyph                  ?‖
   ))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-level-1 'org-level-2 'org-level-3
            'org-level-4 'org-level-5 'org-level-6
            'org-level-7 'org-link
            )
  )

(after! org
  (setq-default
   org-bullets-bullet-list '("⁖")
   org-todo-keyword-faces
   '(
     ("DONE"       :foreground "#7c7c75") ; :weight normal :underline t)
     ("[X]"        :foreground "#7c7c75") ;add-face :weight normal :underline t)
     ("PROJ"       :foreground "#7c7c75") ; :weight normal :underline t)
     ("WAIT"       :foreground "#9f7efe") ; :weight normal :underline t)
     ("[?]"        :foreground "#9f7efe") ; :weight normal :underline t)
     ("STRT"       :foreground "#0098dd") ; :weight normal :underline t)
     ("NEXT"       :foreground "#0098dd") ; :weight normal :underline t)
     ("TODO"       :foreground "#50a14f") ; :weight normal :underline t)
     ("[ ]"        :foreground "#50a14f") ; :weight normal :underline t)
     ("HOLD"       :foreground "#ff6480") ; :weight normal :underline t)
     ("[-]"        :foreground "#ff6480") ; :weight normal :underline t)
     ("ABRT"       :foreground "#ff6480") ; :weight normal :underline t)
     )

   ;; org-priority-faces '((65 :foreground "#e45649")
   ;;                      (66 :foreground "#da8548")
   ;;                      (67 :foreground "#0098dd"))
   )
  )

(custom-set-faces
  '(org-block-begin-line ((t (:background nil))))
  '(org-block-end-line   ((t (:background nil)))))

;; Org Roam Config

(defun tees/org-roam-template-head (file-under)
  (concat "#+TITLE: ${title}\n#+DATE_CREATED: <> \n#+DATE_UPDATED: <> \n#+FIRN_UNDER: " file-under "\n#+FIRN_LAYOUT: default\n\n"))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/Dropbox/wiki"
        org-roam-db-location "~/.org/org-roam.db"
        org-roam-link-title-format "%sº") ;; appends a  `º` to each Roam link.
  (map!
   :desc "Org-Roam-Insert" "C-c i" #'org-roam-insert
   :desc "Org-Roam-Find"   "C-c n" #'org-roam-find-file
   :leader
   :prefix "n"
   :desc "Org-Roam-Insert" "i" #'org-roam-insert
   :desc "Org-Roam-Find"   "/" #'org-roam-find-file
   :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-capture-templates
        `(("p" "project" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-project.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "project")
           :unnarrowed t)
          ("r" "research" entry (function org-roam--capture-get-point)
           ;; "r Entry item!"
           (file "~/.doom.d/templates/org-roam-research.org")
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "research")
           :unnarrowed t)
          ("l" "log" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "log/%<%Y-%m-%d-%H%M>-${slug}"
           :head ,(tees/org-roam-template-head "log")
           :unnarrowed t)
          ("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head ,(tees/org-roam-template-head "general")
           :unnarrowed t)))
  (org-roam-mode +1))

;;; Custom Bindings --

(map!
 ;; -- <GLOBAL> --
 :desc "Switch to 1st workspace" :n  "s-1"   (λ! (+workspace/switch-to 0))
 :desc "Switch to 2nd workspace" :n  "s-2"   (λ! (+workspace/switch-to 1))
 :desc "Switch to 3rd workspace" :n  "s-3"   (λ! (+workspace/switch-to 2))
 :desc "Switch to 4th workspace" :n  "s-4"   (λ! (+workspace/switch-to 3))
 :desc "Switch to 5th workspace" :n  "s-5"   (λ! (+workspace/switch-to 4))
 :desc "Switch to 6th workspace" :n  "s-6"   (λ! (+workspace/switch-to 5))
 :desc "Switch to 7th workspace" :n  "s-7"   (λ! (+workspace/switch-to 6))
 :desc "Switch to 8th workspace" :n  "s-8"   (λ! (+workspace/switch-to 7))
 :desc "Switch to 9th workspace" :n  "s-9"   (λ! (+workspace/switch-to 8))
 :desc "Create workspace"        :n  "s-t"   (λ! (+workspace/new))

; ; -- <LEADER> --

 (:leader
    (:desc "tees" :prefix "v"
     :desc "M-X Alt"                   :n "v" #'execute-extended-command
     :desc "Focus it"                  :n "f" #'focus-mode
     :desc "Correct Spelling at Point" :n "s" #'flyspell-correct-word-before-point)

    ;; additional org roam bindings to `SPC n`
    (:prefix-map ("n" . "notes")
      :desc "Org-Roam-Find"                "/" #'org-roam-find-file
        )

    (:prefix-map ("k" . "lisp")
      :desc "sp-copy"              :n "c" #'sp-copy-sexp
      :desc "sp-kill"              :n "k" #'sp-kill-sexp
      :desc "sp-slurp"             :n "S" #'sp-forward-slurp-sexp
      :desc "sp-barf"              :n "B" #'sp-forward-barf-sexp
      :desc "sp-up"                :n "u" #'sp-up-sexp
      :desc "sp-down"              :n "d" #'sp-down-sexp
      :desc "sp-next"              :n "l" #'sp-next-sexp
      :desc "sp-prev"              :n "h" #'sp-previous-sexp)))

;;' -- Enable gpg stuff --

;; (require 'epa-file)
;; (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
;; (epa-file-enable)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.

;;;  Hooks --

;; update timestamp, if it exists, when saving
(add-hook 'write-file-hooks 'time-stamp)

;; Don't show line numbers in writeroom mode.
(add-hook! 'writeroom-mode-hook
  (display-line-numbers-mode (if writeroom-mode -1 +1)))

(after! cider
  (add-hook 'company-completion-started-hook 'custom/set-company-maps)
  (add-hook 'company-completion-finished-hook 'custom/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'custom/unset-company-maps))

(defun custom/unset-company-maps (&rest unused)
  "Set default mappings (outside of company).
    Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<down>" nil
    "<up>"   nil
    "RET"    nil
    [return] nil
    "C-n"    nil
    "C-p"    nil
    "C-j"    nil
    "C-k"    nil
    "C-h"    nil
    "C-u"    nil
    "C-d"    nil
    "C-s"    nil
    "C-S-s"   (cond ((featurep! :completion helm) nil)
                    ((featurep! :completion ivy)  nil))
    "C-SPC"   nil
    "TAB"     nil
    [tab]     nil
    [backtab] nil))

(defun custom/set-company-maps (&rest unused)
  "Set maps for when you're inside company completion.
    Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<down>" #'company-select-next
    "<up>" #'company-select-previous
    "RET" #'company-complete
    [return] #'company-complete
    "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
    "C-n"     #'company-select-next
    "C-p"     #'company-select-previous
    "C-j"     #'company-select-next
    "C-k"     #'company-select-previous
    "C-h"     #'company-show-doc-buffer
    "C-u"     #'company-previous-page
    "C-d"     #'company-next-page
    "C-s"     #'company-filter-candidates
    "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                    ((featurep! :completion ivy)  #'counsel-company))
    "C-SPC"   #'company-complete-common
    "TAB"     #'company-complete-common-or-cycle
    [tab]     #'company-complete-common-or-cycle
    [backtab] #'company-select-previous    ))
