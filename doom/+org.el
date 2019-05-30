;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-

(after! org
  (map! :map evil-org-mode-map
        :localleader
        :desc "Create_Todo" :nve     "o"  #'org-todo
        :desc "Schedule"    :nve     "s"  #'org-schedule
        :desc "Deadline"    :nve     "d"  #'org-deadline
        :desc "Refile"      :nve     "r"  #'org-refile
        :desc "Filter"      :nve     "f"  #'org-match-sparse-tree
        :desc "Log"         :nve     "l"  #'org-add-note
        :desc "Tag          heading" :nve "t" #'org-set-tags-command)

  (setq wiki-path "~/Dropbox/wiki/")
  (setq agenda-and-refile-targets
        '(("wiki.org" :maxlevel . 1)
          ("todo.org"  :maxlevel . 1)
          ("someday.org" :maxlevel . 1)
          ("calendar.org" :maxlevel . 1)
          ("projects.org" :maxlevel . 1)))

  (toggle-truncate-lines)

  (setq
   org-directory                      wiki-path
   org-default-notes-file             (concat wiki-path "index.org")
   ;; org refile things
   org-refile-targets                 '(("wiki.org" :maxlevel . 1) ("todo.org"  :maxlevel . 1) ("projects.org" :maxlevel . 1))
   org-outline-path-complete-in-steps nil          ; Refile in a single go
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-use-outline-path        'file            ; Show/full/paths for refiling
   ;;
   org-agenda-files                   (list wiki-path)
   org-agenda-use-time-grid nil
   org-fontify-whole-heading-line     nil
   org-tags-column                    80
   org-startup-truncated              t
   org-log-into-drawer                t
   org-ellipsis " ▼ "
   org-log-done                       t
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




  (add-hook 'org-mode-hook #'toggle-word-wrap) ; get line wrapping working
  )

;; Clock time keeping stuff --
;; stolen from : https://emacs.stackexchange.com/questions/32178/how-to-create-table-of-time-distribution-by-tags-in-org-mode/32182
