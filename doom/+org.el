;;; ~/Development/dotfiles/doom/+org.el -*- lexical-binding: t; -*-



(after! org
		(map! :map evil-org-mode-map
								:localleader
								:desc "Create/Edit Todo" :nve "o" #'org-todo
								:desc "Schedule" :nve "s" #'org-schedule
								:desc "Deadline" :nve "d" #'org-deadline
								:desc "Refile" :nve "r" #'org-refile
								:desc "Filter" :nve "f" #'org-match-sparse-tree
								:desc "Add note to logbook" :nve "l" #'org-add-note
								:desc "Tag heading" :nve "t" #'org-set-tags-command)

		(setq wiki-path "~/Dropbox/wiki/")
		(toggle-truncate-lines)

		(setq
			org-directory                      wiki-path
			org-default-notes-file             (concat wiki-path "index.org")
			org-refile-targets                 '(("index.org" :maxlevel . 1) ("todo.org"  :maxlevel . 1))
			org-outline-path-complete-in-steps nil          ; Refile in a single go
			org-refile-use-outline-path        t            ; Show/full/paths for refiling
			org-agenda-files                   (list wiki-path)
			org-fontify-whole-heading-line     nil
			org-tags-column 80
			org-ellipsis " ↴ "
			)

		;; ---- CUSTOM CAPTURE TEMPLATES
		(add-to-list 'org-capture-templates
															'("nB" "New Book"
																	table-line  ; type
																	(file "books.org") ; target
																	"|%U||XXXXXXXX|%^{pages}|%^{comment}|" ; template
																	:prepend t)) ; properties

		;; SELL     (k) Sell template
		;; (add-to-list 'org-capture-templates
		;; 													("s" "SELL      (k) Sell" entry (file "inbox.org")
		;; 														"* SELL %?
		;; :PROPERTIES:
		;; :Cost:
		;; :Paid:
		;; :Method:   [[fin:%^{Method|Wells Fargo Credit Account|Wells Fargo Checking Account|Wells Fargo Debit Account|GE Capital Credit Card}][%\\1]]
		;; :Merchant: [[peo:%^{Merchant}][%\\2]]
		;; :Link:
		;; :Quantity:
		;; :Via:
		;; :Note:
		;; :END:
		;; :LOGBOOK:
		;; - State \"SELL\"       from \"\"           %U
		;; :END:" :empty-lines 1)
		;; 													)


		;; (add-to-list 'org-capture-templates
  ;; ;; CONSUME  (r) Consume template
  ;;  ("mr" "CONSUME   (r) Consume org-protocol" entry (file "inbox.org")
  ;;   "* CONSUME [[%:link][%:description]]
  ;; :PROPERTIES:
  ;; :Creator:  %:creator
  ;; :Created:  %:description
  ;; :Source:   %:source
  ;; :Via:      %:via
  ;; :Link:     %:link
  ;; :Date:     %:date
  ;; :Note:     %:note
  ;; :END:
  ;; :LOGBOOK:
  ;; - State \"CONSUME\"    from \"\"           %U
  ;; :END:
  ;; %:initial" :empty-lines 1)

		;; 													)

		(custom-set-faces '(org-level-1 ((t (:background nil :bold t)))))

		)


;; Clock time keeping stuff --
;; stolen from : https://emacs.stackexchange.com/questions/32178/how-to-create-table-of-time-distribution-by-tags-in-org-mode/32182
