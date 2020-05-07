;;; Startup Calls --

(menu-bar-mode t)
(fringe-mode 0)
;; stop async buffer from popping up (https://emacs.stackexchange.com/a/5554)
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;;; Variable overrides --

(setq-default
 async-shell-command-buffer   'rename-buffer  ; stop async buffer from bothering me when clocking.
 _wiki-path                   "~/Dropbox/wiki/"
 avy-all-windows              'all-frames
 css-indent-offset             2
 deft-directory               _wiki-path
 doom-font                     (font-spec :family "Iosevka" :size 14 :weight 'regular)
 doom-variable-pitch-font      (font-spec :family "IBM Plex Sans" :size 12)
 doom-theme                    'doom-spacegrey
 global-whitespace-mode        0
 js-indent-level               2
 js2-basic-offset              2
 js2-bounce-indent-p           nil
 js2-highlight-level           3
 line-spacing                  2
 projectile-project-search-path '("~/Projects" "~/Development")
 time-stamp-active             t
 time-stamp-format             "%04y-%02m-%02d %02H:%02M:%02S"
 web-mode-code-indent-offset   2
 web-mode-css-indent-offset    2
 web-mode-markup-indent-offset 2
 web-mode-script-padding       2
 web-mode-style-padding        2
 which-key-idle-delay          0.2
 which-key-idle-delay          0.2
 writeroom-width               90
 counsel-rg-base-command       "rg -i -M 160 --no-heading --line-number --color never %s ." ;; stop rg crashing on long files.
 +zen-text-scale               0
)

;; (after! centaur-tabs
;;   (centaur-tabs-mode -1))

;;   (setq centaur-tabs-height 34
;;         centaur-tabs-set-icons t
;;         centaur-tabs-modified-marker " ~ "
;;         centaur-tabs-close-button " × "
;;         centaur-tabs-icon-v-adjust -0.03
;;         centaur-tabs-icon-scale-factor 0.6
;;         centaur-tabs-set-bar 'above)
;;         centaur-tabs-gray-out-icons 'buffer
;;   (centaur-tabs-change-fonts "IBM Plex Sans" 100))

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
 org-agenda-files              '("~/Dropbox/wiki/inbox.org" "~/Dropbox/wiki/priv/work.org")
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
   org-refile-targets                     '((+org/opened-buffer-files :maxlevel . 2))
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
   org-attach-id-dir                      "data/attachments/"
   org-bullets-bullet-list                '("⁖")
   org-log-done                           t
   org-log-into-drawer                    t
   org-outline-path-complete-in-steps     nil ; refile easy
   )
  )

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

;; I customize this for Firn usage.
(after! org-download
  (setq
   org-download-link-format               (concat "[[" org-attach-id-dir "%s]]\n")))

;;; Org Agenda

(after! org
  (set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.5 :select t :ttl nil))

(use-package! org-super-agenda :commands (org-super-agenda-mode))

(after! org-agenda
  (org-super-agenda-mode)
  ;; (set-popup-rule! "^\\*Org Agenda.*" :slot -1 :size 190  :select t)
  (setq
   org-agenda-include-deadlines t
   org-agenda-start-with-log-mode t
   org-agenda-span 3
   org-agenda-block-separator nil
   org-agenda-start-day "+0d"
   org-agenda-use-time-grid nil
   org-global-properties '(("Effort_ALL" . "0 0:10 0:20 0:30 0:45 1:00 1:30 2:00 3:00 4:00 6:00 8:00 10:00 20:00"))
   org-agenda-tags-column 120
   org-agenda-compact-blocks t)
  )

(setq org-agenda-custom-commands
      '(("a" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-files '("~/Dropbox/wiki/inbox.org"))
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :scheduled today
                          :deadline today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-agenda-files '("~/Dropbox/wiki/inbox.org"))
                       (org-super-agenda-groups
                        '((:name "Low effort" :effort< "1:00")
                          (:name "Overdue" :deadline past :face error :order 7)
                          (:name "Unscheduled Tasks" :todo "TODO" :scheduled nila  :order 8)))))))

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

          (todo "" ((org-agenda-overriding-header "_____________________________________________________________________________")
                    (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                    (org-super-agenda-groups
                     '(
                       (:name "BLOCKED" :todo  "WAIT" :todo "HOLD" :order 30)
                       (:name "TASKS" :todo "TODO")
                       ;; (:name "REVIEWS" :tag "review") ;; this isn't working.
                       (:discard (:anything t))))))
          ;; Alternative to not getting the `(:tag "review")'
          (tags "review" ((org-agenda-overriding-header "")
                          (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                          (org-super-agenda-groups
                           '((:name "REVIEWS" :tag "review") ;; this isn't working.
                             (:discard (:anything t))))))))

        ("ww" "Work Week Review"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday 0)
                      (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                      (org-agenda-start-with-log-mode t)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))))
          (todo "" ((org-agenda-overriding-header "_____________________________________________________________________________")
                    (org-agenda-files '("~/Dropbox/wiki/priv/work.org"))
                    (org-super-agenda-groups
                     '((:name "BLOCKED" :todo  "WAIT" :todo "HOLD")
                       (:name "TASKS" :todo "TODO")
                       (:discard (:anything t))))))))))

;; Org Roam Config

(defun tees/org-roam-template-head (file-under)
 (concat "#+TITLE: ${title}\n#+DATE_CREATED: <> \n#+DATE_UPDATED: <> \n#+FIRN_UNDER: " file-under "\n#+FIRN_LAYOUT: default\n\n"))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/Dropbox/wiki"
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
   org-ellipsis                           " • " ;; " ⇢ " ;; ;; " ⋱ " ;;
   org-fontify-whole-heading-line         nil
   org-tags-column                        80
   org-image-actual-width                 400 ; set the width of inline images.
   org-habit-completed-glyph              ?✓
   org-habit-show-all-today               t
   org-habit-today-glyph                  ?‖
   ))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
          'org-level-1
          'org-level-2
          'org-level-3
          'org-level-4
          'org-level-5
          'org-level-6
          'org-level-7
          'org-link
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
     ("[ ]"       :foreground "#50a14f" ) ; :weight normal :underline t)
     ("HOLD"       :foreground "#ff6480") ; :weight normal :underline t)
     ("[-]"        :foreground "#ff6480") ; :weight normal :underline t)
     ("ABRT"       :foreground "#ff6480") ; :weight normal :underline t)
     )

   org-priority-faces '((65 :foreground "#e45649")
                        (66 :foreground "#da8548")
                        (67 :foreground "#0098dd"))
   )
)

;;; Custom Bindings --

(map!

 ; -- <GLOBAL> --

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

 ; -- <LEADER> --

 (:leader
    (:desc "tees" :prefix "v"
     :desc "M-X Alt"                   :n "v" #'execute-extended-command
     :desc "Toggle Centaur Tabs"       :n "t" #'centaur-tabs-mode
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
