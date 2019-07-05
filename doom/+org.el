;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-
;;;



(after! org
  ;; Org mode mapping
  ;; (map! :map evil-org-mode-map
  ;;       :localleader
  ;;       :desc "Create_Todo" :nve     "o"  #'org-todo
  ;;       :desc "Schedule"    :nve     "s"  #'org-schedule
  ;;       :desc "Deadline"    :nve     "d"  #'org-deadline
  ;;       :desc "Refile"      :nve     "r"  #'org-refile
  ;;       :desc "Filter"      :nve     "f"  #'org-match-sparse-tree
  ;;       :desc "Log"         :nve     "l"  #'org-add-note
  ;;       :desc "Tag          heading" :nve "t" #'org-set-tags-command)

  (org-super-agenda-mode)
  (setq wiki-path "~/Development/wiki/")
  (setq agenda-and-refile-targets
        '(("wiki.org"     :maxlevel . 1)
          ("todo.org"     :maxlevel . 1)
          ("someday.org"  :maxlevel . 1)
          ("calendar.org" :maxlevel . 1)))

  (toggle-truncate-lines)

  (setq
   org-directory                      wiki-path
   org-default-notes-file             (concat wiki-path "index.org")
   ;; org refile things
   org-refile-targets                 '(("wiki.org" :maxlevel . 1) ("todo.org"  :maxlevel . 1) ("projects.org" :maxlevel . 1) ("someday.org" :maxlevel . 1))
   org-outline-path-complete-in-steps nil          ; Refile in a single go
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path        'file            ; Show/full/paths for refiling
   org-fontify-whole-heading-line     -1
   org-agenda-files                   (list wiki-path)
   org-fontify-whole-heading-line     nil
   org-tags-column                    80
   org-startup-truncated              t
   org-log-into-drawer                t
   org-cycle-separator-lines          -1
   org-ellipsis " ▼ "
   org-log-done                       t
   ;; org-level-color-stars-only         t

   ;; elfeed things
   elfeed-search-filter "@1-week-ago"

   )

  ;; -- org agenda -----------------------------------------------------------------------------------------

  (setq
   org-agenda-span 'day
   org-agenda-start-day "+0d"
   ;; org-agenda-use-time-grid nil
   ;; org-agenda-skip-scheduled-if-done t
   )

  ;; super agenda configuration
  (setq org-super-agenda-groups
        '(
          (:name "Overdue"    :deadline past)
          (:name "Today"      :time-grid t :scheduled today :deadline today)
          (:name "Important"  :priority "A") ;; Doesn't work.
          (:name "Due soon"   :deadline future)
          (:name "Habits"     :habit t)
          (:name "Quick Picks" :effort< "1:00") ;; doesn't work.
          (:habit t)
          ))

  (after! org-agenda
    (org-super-agenda-mode)
    (map! :map org-agenda-mode-map
          ;; :localleader
          :desc "Forward"       :nve   "]"  #'org-agenda-later
          :desc "Backward"      :nve   "["  #'org-agenda-earlier
          :desc "Month View"    :nve   "m"  #'org-agenda-month-view
          :desc "Week View"     :nve   "w"  #'org-agenda-week-view
          :desc "Day View"      :nve   "d"  #'org-agenda-day-view
          :desc "Filter by tag" :nve   "f"  #'org-agenda-filter-by-tag
          )

    (set-popup-rule! "^\\*Org Agenda.*" :slot -1 :size 190  :select t)
    (after! evil-snipe (push 'org-agenda-mode evil-snipe-disabled-modes))
    (set-evil-initial-state! 'org-agenda-mode 'normal)
    )


  ;; ---- CUSTOM CAPTURE TEMPLATES ------------------------------------------------

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

  (add-to-list 'org-capture-templates '("i" "Inbox" entry (file "inbox.org") "* %u %?\n%i\n" :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates '("l" "Log" entry (file+datetree "log.org.gpg") "**** %U %^{Title} %(org-set-tags-command) \n%?" :prepend t))
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file "todo.org") "* TODO %?\n%i" :prepend t))
  )


;; ;; HOOKS ------------------------
;; get line wrapping working
(add-hook 'org-mode-hook #'toggle-word-wrap)
;; Save archive file after something is archived. https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=887332
(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)
