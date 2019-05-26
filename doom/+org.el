;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-



(after! org
  (map! :map evil-org-mode-map
        :localleader
        :desc "Create/Edit Todo" :nve "o" #'org-todo
        :desc "Schedule" :nve "s" #'org-schedule
        :desc "Deadline" :nve "d" #'org-deadline
        :desc "Refile" :nve "r" #'org-refile
        :desc "Filter" :nve "f" #'org-match-sparse-tree
        :desc "Tag heading" :nve "t" #'org-set-tags-command)

  (setq wiki-path "~/Development/wiki/")

  (setq
   org-directory                      wiki-path
   org-default-notes-file             (concat wiki-path "index.org")
   org-refile-targets '(("index.org" :maxlevel . 1) ("todo.org"  :maxlevel . 1))
   org-outline-path-complete-in-steps nil   ; Refile in a single go
   org-refile-use-outline-path        t            ; Show/full/paths for refiling
   org-agenda-files                   (list wiki-path)
   org-fontify-whole-heading-line     nil
   org-tags-column 80
   )

  (custom-set-faces '(org-level-1 ((t (:background nil :bold t)))))

)


;; Clock time keeping stuff --
;; stolen from : https://emacs.stackexchange.com/questions/32178/how-to-create-table-of-time-distribution-by-tags-in-org-mode/32182
