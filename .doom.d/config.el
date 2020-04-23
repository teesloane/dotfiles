;;; Startup Calls

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

(after! centaur-tabs
  (centaur-tabs-mode 1)
  (setq centaur-tabs-height 36
        centaur-tabs-set-icons t
        centaur-tabs-modified-marker "~"
        centaur-tabs-close-button "×"
        centaur-tabs-set-bar 'above)
        centaur-tabs-gray-out-icons 'buffer
  ;; (centaur-tabs-change-fonts "P22 Underground Book" 160)
  )

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

;;; Custom Bindings --

(map!

 ;; -- <GLOBAL> ---------------------------------------------------------------

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

 ;; -- <LEADER> ----------------------------------------------------------------

 (:leader
    (:desc "tees" :prefix "v"
     :desc "M-X Alt"             :n "v" #'execute-extended-command
     :desc "Correct Spelling at Point" :n "s" #'flyspell-correct-word-before-point )

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

;; -- Enable gpg stuff ---------------------------------------------------------
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; disable caching of passphrases.

;;;  Hooks --
(add-hook 'write-file-hooks 'time-stamp) ; update timestamp, if it exists, when saving

;; Org Directory
(setq
 ;; org-agenda-files              (list _wiki-path)
 org-default-notes-file        (concat _wiki-path "index.org")
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
   org-refile-targets                     '((+org/opened-buffer-files :maxlevel . 4))
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
   ;; org-habit-show-habits-only-for-today   nil
   org-agenda-skip-deadline-if-done       t
   org-agenda-skip-scheduled-if-done      t
   org-agenda-span                        'day
   org-agenda-start-day                   "+0d"
   org-attach-id-dir                      "data/attachments/"
   org-bullets-bullet-list                '("⁖")
   org-log-done                           t
   org-log-into-drawer                    t
   org-outline-path-complete-in-steps     nil ; refile easy
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets                     '((+org/opened-buffer-files :maxlevel . 4))
   org-refile-use-outline-path            'file ; Show/full/paths for refiling
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

  (add-to-list 'org-capture-templates '("i" "Inbox" entry (file "inbox.org") "* %u %?\n%i\n" :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates '("l" "Log" entry (file+datetree "log.org.gpg") "**** %U %^{Title} %(org-set-tags-command) \n%?" :prepend t))
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file "inbox.org") "* TODO %?\n%i" :prepend t)))

;; I customize this for Firn usage.
(after! org-download
  (setq
   org-download-link-format               (concat "[[" org-attach-id-dir "%s]]\n")))

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

;;; Org - Clocking

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

(setq
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-finished-sound-args "-volume 0.3"
 org-pomodoro-long-break-sound-args "-volume 0.3"
 org-pomodoro-short-break-sound-args "-volume 0.3"
 )

(after! org
  (setq
   line-spacing                           3
   org-cycle-separator-lines 2
   org-bullets-bullet-list                '("⁖")
   org-startup-truncated                  t
   org-ellipsis                           " • " ;; " ⇢ " ;; ;; " ⋱ " ;;
   org-fontify-whole-heading-line         nil
   org-tags-column                        80
   org-image-actual-width                 350 ; set the width of inline images.
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
          )
  )

(defun tees/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t))


;; This doesn't really interop well with doom's configuration of write room mode anymore.
(defun tees/write ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Iosevka" :height 140)) ; set the font
  (setq
    writeroom-width         90    ; set width of writeroom mode
    writeroom-maximize-window nil
    indent-tabs-mode        t     ; use tabs for indentation
    tab-width               2     ; set tab width to 2 FIXME
    writeroom-mode-line     nil   ; don't show the modeline
    truncate-lines          nil   ; wrap lines?
    line-spacing            5     ; set line spacing
    global-hl-line-mode     nil   ; Turn off line highlight
    display-line-numbers    nil)  ; don't show line numbers
  (fringe-mode              0)    ; don't show fringe.
  (set-fill-column          90)   ; set width of fill column (for text wrapping.)
  (auto-fill-mode           0)    ; disable line breaking.
  (flyspell-mode)                 ; spell checkin'
  (company-mode             0)    ; disable completion.
  (linum-mode               0)    ; turn off  line  numbers (dooum style.)
  (global-linum-mode        0)    ; turn off  line  numbers again.
  (hl-line-mode             0)    ; stop highlighting stuff!
  (writeroom-mode           1)    ; go into write room   mode.
  (visual-line-mode         1)    ; don't know.
  (blink-cursor-mode)                      ; let's blink that cursor.
  (run-at-time "1 sec" nil #'toggle-frame-fullscreen))
