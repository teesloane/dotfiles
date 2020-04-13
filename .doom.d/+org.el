;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-
;;;

(setq refile-targets '("wiki.org" "todo.org" "projects.org" "someday.org"))

(defun my-org-files-list ()
  (delq nil
        (mapc (lambda (buffer)
                (buffer-file-name buffer))
              (org-buffer-list 'files t))))

(after! org

  ;; org variables not related to directories.
  (setq
   ;; org-habit-show-habits-only-for-today   nil
   elfeed-search-filter                   "@1-week-ago"
   line-spacing                           3
   org-agenda-skip-deadline-if-done       t
   org-agenda-skip-scheduled-if-done      t
   org-agenda-span                        'day
   org-agenda-start-day                   "+0d"
   org-attach-id-dir                      "data/attachments/"
   org-download-link-format               (concat "[[" org-attach-id-dir "%s]]\n")
   org-bullets-bullet-list                '("⁖")
   org-cycle-separator-lines              -1
   org-ellipsis                           " • " ;; " ⇢ " ;; ;; " ⋱ " ;;
   org-fontify-whole-heading-line         nil
   org-habit-completed-glyph              ?✓
   org-habit-show-all-today               t
   org-habit-today-glyph                  ?‖
   org-log-done                           t
   org-image-actual-width                 350
   org-log-into-drawer                    t
   org-outline-path-complete-in-steps     nil ; refile easy
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets                      '((my-org-files-list :maxlevel . 2))
   org-refile-use-outline-path            'file ; Show/full/paths for refiling
   org-startup-truncated                  t
   org-tags-column                        80
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
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file "inbox.org") "* TODO %?\n%i" :prepend t)))

;; turn org-download links into simple relative links.
(after! org-download
  (setq
   org-attach-id-dir                      "./data/attachments/"
   org-download-link-format               (concat "[[" org-attach-id-dir "%s]]\n")))

;; HOOKS -----------------------------------------------------------------------


;;
;; Hook funcs
;;
(defun tees/async-shell-command-no-window
    (command)
  "Run an async command but don't show it's output.
   src: https://www.reddit.com/r/emacs/comments/9wnxdq/async_shell_command_woes/e9mu5bg"
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun tees/org-clock-query-out ()
  "Ask the user before clocking out.
	This is a useful function for adding to `kill-emacs-query-functions'."
	(if (and
       (featurep 'org-clock)
       (funcall 'org-clocking-p)
       (y-or-n-p "You are currently clocking time, clock out? "))
      (org-clock-out)
    t)) ;; only fails on keyboard quit or error

(defun tees/org-on-clock-in ()
	;; (message "Launching anybar and init'ing clock reminder")
	;; (tees/async-shell-command-no-window "~/.teescripts/org-clock-check.sh run")
  (save-buffer))

(defun tees/org-on-clock-out ()
  "Kill the org-clock-check"
  ;; (tees/async-shell-command-no-window "~/.teescripts/org-clock-check.sh stop")
  (save-buffer))

;; -- Hooks

(add-hook 'kill-emacs-query-functions 'tees/org-clock-query-out)
;; These need to be refactored to not stack async spawned processes.
(add-hook 'org-clock-in-hook #'tees/org-on-clock-in)
(add-hook 'org-clock-out-hook #'tees/org-on-clock-out)

