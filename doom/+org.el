;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-


(setq +org-dir "~/Dropbox/notes/")
(setq org-default-notes-file "~/Desktop/notes/organizer.org")


(after! org
  (map! :map evil-org-mode-map
        :localleader
        :desc "Create/Edit Todo" :nve "o" #'org-todo
        :desc "Schedule" :nve "s" #'org-schedule
        :desc "Deadline" :nve "d" #'org-deadline
        :desc "Refile" :nve "r" #'org-refile
        :desc "Filter" :nve "f" #'org-match-sparse-tree
        :desc "Tag heading" :nve "t" #'org-set-tags-command)

  (setq
   org-refile-targets (quote (("notes.org" :maxlevel . 1) ("todo.org" :maxlevel . 1)))
   org-outline-path-complete-in-steps nil ; Refile in a single go
   org-refile-use-outline-path t          ; Show full paths for refiling
   org-log-done 'time
   org-agenda-files '("~/Dropbox/notes" "~/work/toda/notes")
   org-tags-column 80
   ))

(after! org-bullets (setq org-bullets-bullet-list '("#")))


;; Clock time keeping stuff
;; ;; stolen from : https://emacs.stackexchange.com/questions/32178/how-to-create-table-of-time-distribution-by-tags-in-org-mode/32182
;; ;; for clocking time by task.

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
  (interactive)
  (insert "| Tag | Headline | Time (h) |\n")
  (insert "|     |          | <r>  |\n")
  (let ((tags (plist-get params :tags)))
    (mapcar (lambda (tag)
              (setq params (plist-put params :tags tag))
              (clocktable-by-tag/insert-tag params))
            tags)))

(provide 'clocktable-by-tag)
