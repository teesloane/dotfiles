;; there are crashes on osx using projectil and dired :|
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; set the directory for org notes.
(setq +org-dir "~/Dropbox/notes/")
(setq org-default-notes-file "~/Desktop/notes/organizer.org")
