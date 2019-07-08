;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-
;;;

(setq refile-targets '("wiki.org" "todo.org" "projects.org" "someday.org"))


(after! org
  (org-super-agenda-mode)
  (toggle-truncate-lines)

  (setq
   wiki-path "~/Dropbox/wiki/"
   elfeed-search-filter                   "@1-week-ago"
   org-agenda-files                       (list wiki-path)
   org-agenda-span 'day
   org-agenda-start-day "+0d"
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-cycle-separator-lines              -1
	 ;; org-habit-show-all-today               t
	 ;; org-habit-show-habits-only-for-today   nil
   org-habit-today-glyph ?‖
   org-habit-completed-glyph ?✓
   org-default-notes-file                 (concat wiki-path "index.org")
   org-directory                          wiki-path
   org-ellipsis                           " ▼ "
   org-fontify-whole-heading-line         nil
   org-log-done                           t
   org-log-into-drawer                    t
   org-outline-path-complete-in-steps     nil  ; refile easy
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets                     (mapcar (lambda (l) `(,l :maxlevel . 1)) refile-targets)
   org-refile-use-outline-path            'file ; Show/full/paths for refiling
   org-startup-truncated                  t
   org-tags-column                        80

   )



  ;; super agenda configuration
  (setq org-super-agenda-groups
        '(
          (:name "Overdue"    :deadline past)
          (:name "Today"      :time-grid t :scheduled today :deadline today)
          (:name "Important"  :priority "A") ;; Doesn't work.
          (:name "Due soon"   :deadline future)
          (:name "Habits"     :habit t)
          (:name "Habitszz"     :tag "habit")
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
